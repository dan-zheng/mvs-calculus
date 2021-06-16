import AST

protocol HashableObject: AnyObject, Hashable {}

extension HashableObject {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    return lhs === rhs
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}

/// A nested path reference.
public struct NestedPath {
  /// A path component: either a property name or an array index.
  public enum Component {
    /// A property name.
    case name(String)
    /// An array index.
    case index(Use)
  }

  public var components: [Component] = []

  public init(_ components: [Component] = []) {
    self.components = components
  }

  public mutating func appendComponent(_ component: Component) {
    self.components.append(component)
  }
}

extension NestedPath: Sequence {
  public typealias Element = Component
  public typealias Iterator = Array<Component>.Iterator

  public __consuming func makeIterator() -> Array<Component>.Iterator {
    return components.makeIterator()
  }
}

extension NestedPath: Collection {
  public typealias Index = Array<Component>.Index

  public var startIndex: Index {
    components.startIndex
  }

  public var endIndex: Index {
    components.endIndex
  }

  public var indices: Range<Index> {
    components.indices
  }

  public func index(after i: Index) -> Index {
    components.index(after: i)
  }

  public subscript(position: Index) -> Component {
    _read {
      yield components[position]
    }
    _modify {
      yield &components[position]
    }
  }
}

extension NestedPath: CustomStringConvertible {
  public var description: String {
    var result: String = ""
    for component in components {
      switch component {
      case let .name(name):
        result += ".\(name)"
      case let .index(index):
        result += "[\(index)]"
      }
    }
    return result
  }
}

// A local variable: an argument or an instruction.
public protocol Value {
  var type: Type { get }
  func makeUse() -> Use
}

public typealias BinaryOp = OperExpr.Kind

public enum Use: Hashable {
  case argument(Argument)
  case instruction(Instruction)
  case function(Function)
  case intrinsic(BinaryOp)
}

extension Use {
  var parent: Function? {
    switch self {
    case let .argument(arg):
      return arg.parent
    case let .instruction(inst):
      return inst.parent
    case .function, .intrinsic:
      return nil
    }
  }

  var type: Type {
    switch self {
    case let .argument(arg):
      return arg.type
    case let .instruction(inst):
      return inst.type
    case let .function(f):
      return f.type
    case .intrinsic:
      // TODO: Intrinsics are hardcoded to be binary numeric operations.
      return .func(params: [.float, .float], output: .float)
    }
  }

  /// Returns `true` iff the use represents a function reference.
  var isFunctionReference: Bool {
    switch self {
    case .function, .intrinsic:
      return true
    default:
      return false
    }
  }
}

extension Use: TextOutputStreamable {
  public func write<Target: TextOutputStream>(to target: inout Target) {
    target.write("\(identifier): \(type)")
  }

  public var identifier: String {
    switch self {
    case let .argument(arg):
      return "%\(arg.printedName)"
    case let .instruction(inst):
      return inst.printedName.flatMap { "%\($0)" } ?? "%_"
    case let .function(f):
      return "@\(f.name)"
    case let .intrinsic(i):
      return "@intrinsic.\(i)"
    }
  }
}

public class Argument: Value, HashableObject {
  public var name: String?
  public var type: Type
  public unowned var parent: Function

  public init(name: String?, mutability: MutabilityQualifier, type: Type, parent: Function) {
    self.name = name
    self.type = type
    self.parent = parent
  }

  public var mutability: MutabilityQualifier {
    if case .inout(_) = type {
      return .var
    }
    return .let
  }

  public func makeUse() -> Use {
    .argument(self)
  }

  static prefix func % (_ arg: Argument) -> Use {
    arg.makeUse()
  }
}

extension Argument {
  public var printedName: String {
    if let name = name {
      return name
    }
    let selfIndex = parent.arguments.firstIndex(of: self)!
    return "^\(selfIndex)"
  }
}

public class Module: HashableObject {
  public var functions: [Function]

  public init(functions: [Function] = []) {
    self.functions = functions
  }

  public func findFunction(_ name: String) -> Function? {
    functions.first(where: { $0.name == name })
  }

  func addFunction(_ function: Function) {
    guard !functions.contains(where: { $0.name == function.name }) else {
      fatalError("Module already contains function '\(function.name)'")
    }
    functions.append(function)
  }
}

extension Module: TextOutputStreamable {
  public func write<Target: TextOutputStream>(to target: inout Target) {
    for function in functions {
      function.write(to: &target)
      target.write("\n")
    }
  }
}

/// An SSA function.
public class Function: HashableObject {
  public var name: String
  public var arguments: [Argument]
  public var returnType: Type

  public var instructions: [Instruction] = []

  public init(name: String, arguments: [Argument], returnType: Type) {
    self.name = name
    self.arguments = arguments
    self.returnType = returnType
  }

  public init<C: Collection>(
    name: String, arguments: C, returnType: Type
  ) where C.Element == (name: String?, mutability: MutabilityQualifier, type: Type) {
    self.name = name
    self.returnType = returnType
    self.arguments = []
    for (name, mut, type) in arguments {
      let argument = Argument(name: name, mutability: mut, type: type, parent: self)
      self.arguments.append(argument)
    }
  }

  public func insert(_ instruction: Instruction, at index: Int) {
    self.instructions.insert(instruction, at: index)
    instruction.parent = self
  }

  public func append(_ instruction: Instruction) {
    self.instructions.append(instruction)
    instruction.parent = self
  }
}

extension Function {
  var argumentTypes: [Type] {
    arguments.map { $0.type }
  }

  var type: Type {
    .func(params: argumentTypes, output: returnType)
  }

  var result: Use {
    guard let returnInst = instructions.last,
      case let .return(use) = returnInst.kind
    else {
      fatalError("Expected function's last instruction to be 'return'")
    }
    return use
  }
}

// MARK: - Function cloning
extension Function {
  /// Create clone of function.
  func makeClone(named name: String) -> Function {
    let args = arguments.map { arg in
      (arg.name, arg.mutability, arg.type)
    }
    let newFunc = Function(name: name, arguments: args, returnType: returnType)
    copyContents(to: newFunc)
    return newFunc
  }

  /// Copy instructions to an empty function.
  func copyContents(to other: Function) {
    /// Other function must be empty.
    guard other.instructions.isEmpty else {
      fatalError(
        "Could not copy contents to @\(other.name) because it is not empty.")
    }

    /// Mappings from old IR units to new IR units.
    var newArgs: [Argument: Argument] = [:]
    var newInsts: [Instruction: Instruction] = [:]

    func newUse(from old: Use) -> Use {
      switch old {
      case .function(self):
        return .function(other)
      case .function:
        return old
      case let .argument(arg):
        return %newArgs[arg]!
      case let .instruction(inst):
        return %newInsts[inst]!
      case .intrinsic:
        return old
      }
    }

    /// Clone arguments.
    assert(self.arguments.count == other.arguments.count)
    for (oldArg, newArg) in zip(self.arguments, other.arguments) {
      newArgs[oldArg] = newArg
    }

    /// Clone instructions.
    for oldInst in self.instructions {
      let newInst = Instruction(name: oldInst.name, kind: oldInst.kind, parent: other)
      /// Replace operands with new uses.
      for oldUse in newInst.operands {
        newInst.substitute(newUse(from: oldUse), for: oldUse)
      }
      /// Insert instruction into mapping.
      newInsts[oldInst] = newInst
      other.append(newInst)
    }

    for newInst in other.instructions {
      assert(
        newInst.parent == other,
        "The parent of instruction \(newInst) must be its containing function \(other)")
      for operand in newInst.operands where operand.parent != nil {
        assert(
          operand.parent == other,
          "The parent of operand \(operand) must be its containing function \(other)")
      }
    }

    print("Newly cloned:")
    print(other)
  }
}

extension Function: TextOutputStreamable {
  public func write<Target: TextOutputStream>(to target: inout Target) {
    target.write("func ")
    target.write("@\(name)(")
    for (i, arg) in arguments.enumerated() {
      target.write("\(arg.printedName): \(arg.type)")
      if i != arguments.endIndex - 1 {
        target.write(", ")
      }
    }
    /*
    for (i, arg) in arguments.enumerated() {
      let mutability = arg.mutability == .let ? "" : "inout "
      target.write("\(arg.printedName): \(mutability)\(arg.type)")
      if i != arguments.endIndex - 1 {
        target.write(", ")
      }
    }
    */
    target.write(") -> \(returnType) {\n")
    for inst in instructions {
      "  ".write(to: &target)
      inst.write(to: &target)
      target.write("\n")
    }
    target.write("}")
  }
}

@frozen
public enum NumericBinaryOp: Hashable {
  case add, sub, mul, div
}

@frozen
public enum BooleanBinaryOp: Hashable {
  case eq, ne
  case lt, le, ge, gt
}

extension NumericBinaryOp {
  var binaryOp: BinaryOp {
    switch self {
    case .add:
      return .add
    case .sub:
      return .sub
    case .mul:
      return .mul
    case .div:
      return .div
    }
  }

  func callAsFunction(_ lhs: EvaluatedValue, _ rhs: EvaluatedValue) -> EvaluatedValue {
    switch (lhs, rhs) {
    case let (.int(l), .int(r)):
      switch self {
      case .add:
        return .int(l + r)
      case .sub:
        return .int(l - r)
      case .mul:
        return .int(l * r)
      case .div:
        return .int(l / r)
      }
    case let (.float(l), .float(r)):
      switch self {
      case .add:
        return .float(l + r)
      case .sub:
        return .float(l - r)
      case .mul:
        return .float(l * r)
      case .div:
        return .float(l / r)
      }
    case let (.array(l), .array(r)):
      let elements = zip(l, r).map { self($0, $1) }
      return .array(elements)
    case let (.struct(lhsType, lhsProperties), .struct(rhsType, rhsProperties)):
      assert(lhsType == rhsType)
      assert(Set(lhsProperties.keys) == Set(rhsProperties.keys))
      var resultProperties = lhsProperties
      for (key, value) in resultProperties {
        resultProperties[key] = self(value, rhsProperties[key]!)
      }
      return .struct(type: lhsType, properties: resultProperties)
    default:
      fatalError("Unsupported operands")
    }
  }
}

extension BooleanBinaryOp {
  var binaryOp: BinaryOp {
    switch self {
    case .eq:
      return .eq
    case .ne:
      return .ne
    case .lt:
      return .lt
    case .le:
      return .le
    case .ge:
      return .ge
    case .gt:
      return .gt
    }
  }

  func callAsFunction(_ lhs: EvaluatedValue, _ rhs: EvaluatedValue) -> EvaluatedValue {
    switch (lhs, rhs) {
    case let (.int(l), .int(r)):
      switch self {
      case .eq:
        return .int(l == r ? 1 : 0)
      case .ne:
        return .int(l != r ? 1 : 0)
      case .lt:
        return .int(l < r ? 1 : 0)
      case .le:
        return .int(l <= r ? 1 : 0)
      case .ge:
        return .int(l >= r ? 1 : 0)
      case .gt:
        return .int(l > r ? 1 : 0)
      }
    case let (.float(l), .float(r)):
      switch self {
      case .eq:
        return .int(l == r ? 1 : 0)
      case .ne:
        return .int(l != r ? 1 : 0)
      case .lt:
        return .int(l < r ? 1 : 0)
      case .le:
        return .int(l <= r ? 1 : 0)
      case .ge:
        return .int(l >= r ? 1 : 0)
      case .gt:
        return .int(l > r ? 1 : 0)
      }
    case let (.array(l), .array(r)):
      let elements = zip(l, r).map { self($0, $1) }
      return .array(elements)
    case let (.struct(lhsType, lhsProperties), .struct(rhsType, rhsProperties)):
      assert(lhsType == rhsType)
      assert(Set(lhsProperties.keys) == Set(rhsProperties.keys))
      var resultProperties = lhsProperties
      for (key, value) in resultProperties {
        resultProperties[key] = self(value, rhsProperties[key]!)
      }
      return .struct(type: lhsType, properties: resultProperties)
    default:
      fatalError("Unsupported operands")
    }
  }
}

public indirect enum InstructionKind {
  // Atoms
  case int(Int)
  case float(Double)

  // Complex
  case binaryOp(BinaryOp, argumentType: Type)
  case numericBinary(NumericBinaryOp, Use, Use)
  case path(base: Use, path: NestedPath)
  case call(Use, [Use])
  case array([Use])
  case `struct`(name: String, type: Type?, arguments: [Use])
  case assign(lhsBase: Use, lhsPath: NestedPath, rhs: Use)
  case accumulate(lhsBase: Use, lhsPath: NestedPath, rhs: Use)

  // Bindings
  case alloc(Type)
  case `let`(Type, value: Use)

  case `return`(Use)
}

extension InstructionKind {
  public var type: Type {
    switch self {
    case .int:
      return .int
    case .float:
      return .float
    case let .binaryOp(op, argumentType):
      guard let type = op.type(forOperandsOfType: argumentType) else {
        fatalError("Could not deduce binary operation type from argument type")
      }
      return type
    case let .numericBinary(_, lhs, rhs):
      assert(lhs.type == rhs.type)
      return lhs.type
    case let .path(base: base, path: path):
      var pathType = base.type
      for component in path.components {
        switch component {
        case let .name(propertyName):
          guard case let .struct(name: _, props: properties) = pathType else {
            fatalError("Type must be a struct type")
          }
          guard let property = properties.first(where: { $0.name == propertyName }) else {
            fatalError("Struct type must have property with given property name")
          }
          pathType = property.type
        case .index:
          guard case let .array(elem: elementType, count: _) = pathType else {
            fatalError("Type must be an array type")
          }
          pathType = elementType
        }
      }
      return pathType
    case let .call(callee, _):
      guard case let .func(params: _, output: output) = callee.type else {
        fatalError("Callee type: \(callee.type)")
      }
      return output
    case let .array(elements):
      let elementType = elements.first?.type ?? .unit
      return .array(elem: elementType, count: elements.count)
    case .struct(name: _, let type, arguments: _):
      return type!
    case let .alloc(type):
      return type
    case let .`let`(type, value: _):
      return type
    case .assign, .accumulate, .return:
      return .unit
    }
  }
}

extension NestedPath {
  public var operands: [Use] {
    var operands: [Use] = []
    for component in self {
      // Index path components have an operand.
      if case let .index(i) = component {
        operands.append(i)
      }
    }
    return operands
  }
}

extension InstructionKind {
  public var operands: [Use] {
    switch self {
    case .int, .float, .binaryOp:
      return []
    case let .numericBinary(_, lhs, rhs):
      return [lhs, rhs]
    case let .path(base: base, path: path):
      return [base] + path.operands
    case let .call(callee, arguments):
      return [callee] + arguments
    case let .array(elements):
      return elements
    case let .struct(name: _, type: _, arguments: arguments):
      return arguments
    case let .assign(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
      return [lhsBase, rhs] + lhsPath.operands
    case let .accumulate(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
      return [lhsBase, rhs] + lhsPath.operands
    case .alloc:
      return []
    case let .`let`(_, value: v):
      return [v]
    case let .return(use):
      return [use]
    }
  }
}

extension Instruction {
  public var operands: [Use] {
    kind.operands
  }
}

extension InstructionKind {
  public var mustWriteToMemory: Bool {
    switch self {
    case .alloc, .`let`, .assign, .accumulate:
      return true
    default:
      return false
    }
  }

  public var isReturn: Bool {
    switch self {
    case .return:
      return true
    default:
      return false
    }
  }
}

extension Sequence {
  /// Elements' descriptions joined by comma
  public var joinedDescription: String {
    return description(joinedBy: ", ")
  }

  /// Elements' descriptions joined
  public func description(joinedBy separator: String) -> String {
    return map { "\($0)" }.joined(separator: separator)
  }
}

extension InstructionKind: TextOutputStreamable {
  public func write<Target: TextOutputStream>(to target: inout Target) {
    switch self {
    case let .int(i):
      target.write("\(i): \(type)")
    case let .float(f):
      target.write("\(f): \(type)")
    case let .binaryOp(op, argumentType: _):
      target.write("intrinsic.\(op)")
    case let .numericBinary(op, lhs, rhs):
      target.write("\(op) \(lhs) \(rhs)")
    case let .path(base: base, path: path):
      target.write("\(base.identifier)\(path)")
    case let .call(callee, arguments):
      var retType: Type = .error
      if case let .func(params: _, output: fRetType) = callee.type {
        retType = fRetType
      }
      target.write("\(callee.identifier)(\(arguments.joinedDescription)): \(retType)")
    case let .array(elements):
      target.write("[\(elements.joinedDescription)]")
    case let .struct(name: name, type: _, arguments: arguments):
      target.write("struct \(name)(\(arguments.joinedDescription))")
    case let .assign(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
      target.write("\(lhsBase.identifier)\(lhsPath) = \(rhs)")
    case let .accumulate(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
      target.write("\(lhsBase.identifier)\(lhsPath) += \(rhs)")
    case let .alloc(type):
      target.write("alloc \(type)")
    case let .`let`(type, value: value):
      target.write("init \(type) \(value)")
    case let .return(use):
      target.write("return \(use)")
    }
  }
}

prefix operator %

public class Instruction: Value, HashableObject {
  public var name: String?
  public var kind: InstructionKind
  public var parent: Function

  public init(name: String?, kind: InstructionKind, parent: Function) {
    self.name = name
    self.kind = kind
    self.parent = parent
  }

  public var type: Type {
    kind.type
  }

  public func makeUse() -> Use {
    .instruction(self)
  }

  static prefix func % (_ i: Instruction) -> Use {
    i.makeUse()
  }
}

extension Instruction {
  public var printedName: String? {
    return name ?? (type.isUnit ? nil : "\(parent.instructions.firstIndex(of: self)!)")
  }

  public func removeFromParent() {
    guard let index = parent.instructions.firstIndex(of: self) else { return }
    parent.instructions.remove(at: index)
  }
}

// MARK: - Substitution utilities

extension Instruction {
  public func substitute(_ newUse: Use, for use: Use) {
    kind = kind.substituting(newUse, for: use)
  }
}

extension InstructionKind {
  /// Substitutes a new use for an old use.
  public func substituting(_ new: Use, for old: Use) -> InstructionKind {
    let condSubst = { $0 == old ? new : $0 }
    switch self {
    case let .numericBinary(op, lhs, rhs):
      return .numericBinary(op, condSubst(lhs), condSubst(rhs))
    case let .path(base: base, path):
      func substPathComponent(_ component: NestedPath.Component) -> NestedPath.Component {
        if case let .index(i) = component {
          return .index(condSubst(i))
        }
        return component
      }
      return .path(base: condSubst(base), path: NestedPath(path.map(substPathComponent)))
    case let .call(fn, arguments):
      return .call(condSubst(fn), arguments.map(condSubst))
    case let .array(elements):
      return .array(elements.map(condSubst))
    case let .struct(name: name, type: type, arguments: arguments):
      return .struct(name: name, type: type, arguments: arguments.map(condSubst))
    case let .assign(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
      return .assign(lhsBase: condSubst(lhsBase), lhsPath: lhsPath, rhs: condSubst(rhs))
    case let .accumulate(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
      return .accumulate(lhsBase: condSubst(lhsBase), lhsPath: lhsPath, rhs: condSubst(rhs))
    case .return(old):
      return .return(new)
    default:
      return self
    }
  }
}

extension Instruction: TextOutputStreamable {
  public func write<Target: TextOutputStream>(to target: inout Target) {
    if let name = printedName {
      target.write("%\(name) = ")
    }
    kind.write(to: &target)
  }
}

extension Instruction {
  public func replaceAllUsesWith(_ use: Use) {
    let dfg = DataFlowGraphAnalysis.run(on: parent)
    for successor in dfg.successors(of: %self) {
      successor.substitute(use, for: %self)
    }
  }
}

public class IRBuilder {
  public let function: Function

  public var insertionIndex: Int? = nil

  public init(function: Function) {
    self.function = function
  }

  @discardableResult
  func buildInstruction(_ kind: InstructionKind, name: String? = nil) -> Instruction {
    let inst = Instruction(name: name, kind: kind, parent: function)
    // If insertion index is defined, insert at the index and advance it.
    if let index = insertionIndex {
      function.insert(inst, at: index)
      insertionIndex = index + 1
    }
    // Otherwise, append instruction to end of function.
    else {
      function.append(inst)
    }
    return inst
  }

  func setInsertionPoint(before inst: Instruction) {
    guard let instIndex = function.instructions.firstIndex(of: inst) else {
      fatalError("Instruction \(inst) does not exist in function \(function)")
    }
    self.insertionIndex = instIndex
  }
}

public indirect enum EvaluatedValue {
  case int(Int)
  case float(Double)
  case array([EvaluatedValue])
  case `struct`(type: Type, properties: [String: EvaluatedValue])
  case binaryFunction(BinaryOp)
}

extension EvaluatedValue {
  public static func zero(_ type: Type) -> EvaluatedValue {
    switch type {
    case .int:
      return .int(0)
    case .float:
      return .float(0)
    case .struct(name: _, let props):
      let properties = props.map { prop in (prop.name, EvaluatedValue.zero(prop.type)) }
      let propertiesDict = Dictionary(uniqueKeysWithValues: properties)
      return .struct(type: type, properties: propertiesDict)
    case .array(let elem, let count):
      let zeroElement = EvaluatedValue.zero(elem)
      let zeroElements = Array(repeating: zeroElement, count: count)
      return .array(zeroElements)
    case .unit, .func, .inout, .error:
      fatalError()
    }
  }

  public func at(_ path: NestedPath) -> EvaluatedValue {
    guard let component = path.first else {
      return self
    }
    let remainingPath = NestedPath(Array(path.components.dropFirst()))
    switch component {
    case let .name(propertyName):
      guard case let .struct(type: _, properties: properties) = self else {
        fatalError()
      }
      let property = properties[propertyName]!
      return property.at(remainingPath)
    case let .index(index):
      guard case let .array(elements) = self else {
        fatalError()
      }
      guard case let .instruction(inst) = index,
        case let .int(i) = inst.kind
      else {
        fatalError("Index \(index) is not an integer")
      }
      let element = elements[i]
      return element.at(remainingPath)
    }
  }

  public func with(_ path: NestedPath, newValue: EvaluatedValue) -> EvaluatedValue {
    guard let component = path.first else {
      return newValue
    }
    let remainingPath = NestedPath(Array(path.components.dropFirst()))
    switch component {
    case let .name(propertyName):
      guard case .struct(type: let type, properties: var properties) = self else {
        fatalError()
      }
      let property = properties[propertyName]!
      let newProperty = property.with(remainingPath, newValue: newValue)
      properties[propertyName] = newProperty
      return .struct(type: type, properties: properties)
    case let .index(index):
      guard case .array(var elements) = self else {
        fatalError()
      }
      guard case let .instruction(inst) = index,
        case let .int(i) = inst.kind
      else {
        fatalError("Index \(index) is not an integer")
      }
      let element = elements[i]
      let newElement = element.with(remainingPath, newValue: newValue)
      elements[i] = newElement
      return .array(elements)
    }
  }
}

extension Function {
  public func evaluated(in environment: [String: EvaluatedValue] = [:], argumentValues: [EvaluatedValue])
    -> EvaluatedValue
  {
    var newEnvironment = environment

    // Set arguments in environment.
    assert(arguments.count == argumentValues.count)
    for (arg, argValue) in zip(arguments, argumentValues) {
      newEnvironment[(%arg).identifier] = argValue
    }

    // Evaluate instructions in order.
    for instruction in instructions {
      // Store result in environment using the instruction name.
      let name = (%instruction).identifier
      switch instruction.kind {
      case let .int(i):
        newEnvironment[name] = .int(i)
      case let .float(f):
        newEnvironment[name] = .float(f)
      case let .binaryOp(op, _):
        newEnvironment[name] = .binaryFunction(op)
      case let .numericBinary(op, lhs, rhs):
        let lhsValue = newEnvironment[lhs.identifier]!
        let rhsValue = newEnvironment[rhs.identifier]!
        let resultValue = op(lhsValue, rhsValue)
        newEnvironment[name] = resultValue
      case let .path(base: base, path: path):
        let baseValue = newEnvironment[base.identifier]!
        let pathValue = baseValue.at(path)
        newEnvironment[name] = pathValue
      case let .call(callee, calleeArgs):
        guard case let .function(f) = callee else {
          fatalError("Callee is not a function")
        }
        let calleeArgValues = calleeArgs.map { newEnvironment[$0.identifier]! }
        let resultValue = f.evaluated(argumentValues: calleeArgValues)
        newEnvironment[name] = resultValue
      case let .array(elements):
        let elementValues = elements.map { newEnvironment[$0.identifier]! }
        newEnvironment[name] = .array(elementValues)
      case let .struct(name: name, type: type, arguments: arguments):
        let structArgValues = arguments.map { newEnvironment[$0.identifier]! }
        guard let structType = type,
          case .struct(name: name, props: let props) = structType
        else {
          fatalError("Type is not a struct type")
        }
        let properties = zip(props, structArgValues).map { prop, arg in (prop.name, arg) }
        let propertiesDict = Dictionary(uniqueKeysWithValues: properties)
        newEnvironment[name] = .struct(type: structType, properties: propertiesDict)
      case let .assign(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
        let lhsBaseValue = newEnvironment[lhsBase.identifier]!
        let rhsValue = newEnvironment[rhs.identifier]!
        let newLhsBaseValue = lhsBaseValue.with(lhsPath, newValue: rhsValue)
        newEnvironment[lhsBase.identifier] = newLhsBaseValue
      case let .accumulate(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs):
        let lhsBaseValue = newEnvironment[lhsBase.identifier]!
        let lhsPathValue = lhsBaseValue.at(lhsPath)
        let rhsValue = newEnvironment[rhs.identifier]!
        let newRhsValue = NumericBinaryOp.add(lhsPathValue, rhsValue)
        let newLhsBaseValue = lhsBaseValue.with(lhsPath, newValue: newRhsValue)
        newEnvironment[lhsBase.identifier] = newLhsBaseValue
      case let .alloc(type):
        newEnvironment[name] = EvaluatedValue.zero(type)
      case let .`let`(_, value: value):
        newEnvironment[name] = newEnvironment[value.identifier]!
      case let .return(use):
        let value = newEnvironment[use.identifier]!
        return value
      }
    }
    fatalError("Did not encounter return instruction")
  }
}

let addFunction = Function(
  name: "add", arguments: [("x", .let, .float), ("y", .let, .float)], returnType: .float)
let subFunction = Function(
  name: "sub", arguments: [("x", .let, .float), ("y", .let, .float)], returnType: .float)
let mulFunction = Function(
  name: "mul", arguments: [("x", .let, .float), ("y", .let, .float)], returnType: .float)

let intrinsicFunctions: [Function] = [
  addFunction, subFunction, mulFunction,
]

let intrinsicFunctionEnvironment: [String: Use] = Dictionary(
  uniqueKeysWithValues: intrinsicFunctions.map { f in (f.name, .function(f)) }
)

/// Translates mvs expressions to lower-level SSA instructions.
public struct InstructionGenerator: ExprVisitor, PathVisitor {
  public typealias ExprResult = Use

  public typealias PathResult = (op: Use, path: NestedPath)

  private var counter: Int = 0

  mutating func gensym() -> String {
    defer { counter += 1 }
    return "x\(counter)"
  }

  public var module: Module = Module()

  /// Per-function mapping from variable names to uses.
  private var environment: [Function: [String: Use]] = [:]

  public var mainFunction: Function
  public var builder: IRBuilder

  public init() {
    self.mainFunction = Function(name: "main", arguments: [], returnType: .unit)
    module.addFunction(self.mainFunction)

    self.environment = [:]
    self.builder = IRBuilder(function: self.mainFunction)
  }

  public mutating func visit(_ program: inout Program) {
    let body = program.entry.accept(&self)
    builder.buildInstruction(.return(body))
  }

  public mutating func visit(_ expr: inout IntExpr) -> Use {
    return %builder.buildInstruction(.int(expr.value))
  }

  public mutating func visit(_ expr: inout FloatExpr) -> Use {
    return %builder.buildInstruction(.float(expr.value))
  }

  public mutating func visit(_ expr: inout ArrayExpr) -> Use {
    var elements: [Use] = []
    for i in expr.elems.indices {
      let element = expr.elems[i].accept(&self)
      elements.append(element)
    }
    return %builder.buildInstruction(.array(elements))
  }

  public mutating func visit(_ expr: inout StructExpr) -> Use {
    var arguments: [Use] = []
    for i in expr.args.indices {
      let arg = expr.args[i].accept(&self)
      arguments.append(arg)
    }
    return %builder.buildInstruction(
      .struct(name: expr.name, type: expr.type, arguments: arguments))
  }

  public mutating func visit(_ expr: inout FuncExpr) -> Use {
    guard case let .func(params: argumentTypes, output: output) = expr.type else {
      fatalError()
    }
    let argumentNames = expr.params.map { $0.name }
    let argumentMutabilities = expr.params.map { $0.mutability }
    var arguments: [(String, MutabilityQualifier, Type)] = []
    for ((name, mutability), type) in zip(zip(argumentNames, argumentMutabilities), argumentTypes) {
      arguments.append((name, mutability, type))
    }
    let oldBuilder = builder

    // Create a new function and set its environment.
    let newFunction = Function(name: gensym(), arguments: arguments, returnType: output)
    self.module.addFunction(newFunction)
    environment[newFunction] = environment[builder.function]
    for arg in newFunction.arguments {
      environment[newFunction, default: intrinsicFunctionEnvironment][arg.printedName] = %arg
    }

    // Generate the body of the new function and emit a return.
    builder = IRBuilder(function: newFunction)
    let body = expr.body.accept(&self)
    builder.buildInstruction(.return(body))

    // Generate a reference to the new function.
    builder = oldBuilder
    return .function(newFunction)
  }

  public mutating func visit(_ expr: inout CallExpr) -> Use {
    var arguments: [Use] = []
    for i in expr.args.indices {
      let arg = expr.args[i].accept(&self)
      arguments.append(arg)
    }
    // Handle calls to specific intrinsic functions.
    if let namePath = expr.callee as? NamePath {
      let name = namePath.name
      // Handle gradient operation.
      if name == "gradient" || name == "gradient2" {
        // Check that gradient call has at least one argument.
        guard case let .function(f) = arguments.first else {
          fatalError("Expected gradient call to have at least one argument")
        }
        // Check that gradient operand has expected number of arguments.
        let expectedArgumentCount: Int
        switch name {
        case "gradient": expectedArgumentCount = 2
        case "gradient2": expectedArgumentCount = 3
        default:
          fatalError("Unexpected gradient function name")
        }
        assert(
          arguments.count == expectedArgumentCount,
          """
          Expected \(name) call to have \(expectedArgumentCount) arguments; \
          instead got \(arguments.count)
          """
        )

        // Get gradient function and call it.
        let gradientFunction = gradient(f)
        let result = %builder.buildInstruction(
          .call(.function(gradientFunction), Array(arguments.dropFirst())))
        print("result: \(result)")
        print(builder.function)
        return result
      }
    }
    let callee = expr.callee.accept(&self)
    return %builder.buildInstruction(.call(callee, arguments))
  }

  public mutating func visit(_ expr: inout InfixExpr) -> Use {
    let lhs = expr.lhs.accept(&self)
    let rhs = expr.rhs.accept(&self)
    let op = expr.oper.accept(&self)
    return %builder.buildInstruction(.call(op, [lhs, rhs]))
  }

  public mutating func visit(_ expr: inout OperExpr) -> Use {
    return .intrinsic(expr.kind)
  }

  public mutating func visit(_ expr: inout InoutExpr) -> Use {
    // TODO: Handle inout expressions.
    fatalError("Unsupported expression: \(expr)")
  }

  public mutating func visit(_ expr: inout BindingExpr) -> Use {
    // Lower the initializer expression of the binding.
    let initializer = expr.initializer.accept(&self)
    // Update the name of the lowered expression to the binding name.
    let name = expr.decl.name
    switch initializer {
    case let .argument(arg):
      arg.name = name
    case let .function(f):
      f.name = name
    case .instruction, .intrinsic:
      break
    }

    // Update environment using the binding name.
    guard let type = expr.initializer.type else {
      fatalError("Initializer expression does not have a type")
    }

    /*
    switch expr.decl.mutability {
    case .let:
      environment[
        builder.function, default: intrinsicFunctionEnvironment][name] = initializer
    case .var:
      let lhsBase = builder.buildInstruction(.alloc(type), name: name)
      _ = builder.buildInstruction(
        .assign(lhsBase: %lhsBase, lhsPath: NestedPath(), rhs: initializer))
      environment[
        builder.function, default: intrinsicFunctionEnvironment][name] = %lhsBase
    }
    */

    // Lower bindings as allocations followed by assignments.
    let lhsBase = builder.buildInstruction(.alloc(type), name: name)
    _ = builder.buildInstruction(
      .assign(lhsBase: %lhsBase, lhsPath: NestedPath(), rhs: initializer))
    environment[
      builder.function, default: intrinsicFunctionEnvironment][name] = %lhsBase
    let body = expr.body.accept(&self)
    return body
  }

  public mutating func visit(_ expr: inout AssignExpr) -> Use {
    let (lhsBase, lhsPath) = expr.lvalue.accept(pathVisitor: &self)
    let rhs = expr.rvalue.accept(&self)
    _ = builder.buildInstruction(
      .assign(lhsBase: lhsBase, lhsPath: lhsPath, rhs: rhs))
    return expr.body.accept(&self)
  }

  public mutating func visit(_ expr: inout ErrorExpr) -> Use {
    fatalError("Unsupported expression: \(expr)")
  }

  public mutating func visit(_ expr: inout NamePath) -> Use {
    let name = expr.name
    // Look up name in the environment.
    guard let use = environment[builder.function]?[name] else {
      fatalError("Variable '\(name)' not found")
    }
    return use
  }

  public mutating func visit(_ expr: inout PropPath) -> Use {
    let base = expr.base.accept(&self)
    let path = NestedPath([.name(expr.name)])
    return %builder.buildInstruction(.path(base: base, path: path))
  }

  public mutating func visit(_ expr: inout ElemPath) -> Use {
    let index = expr.index.accept(&self)
    let base = expr.base.accept(&self)
    let path = NestedPath([.index(index)])
    return %builder.buildInstruction(.path(base: base, path: path))
  }

  public mutating func visit(_ expr: inout GradientExpr) -> Use {
    fatalError()
  }

  // - MARK: Path visitors

  public mutating func visit(path: inout NamePath) -> PathResult {
    var root = path.root
    let rootInstruction = root.accept(&self)
    let path = NestedPath()
    return (op: rootInstruction, path: path)
  }

  public mutating func visit(path: inout ElemPath) -> PathResult {
    let index = path.index.accept(&self)
    let pathComponent = NestedPath.Component.index(index)

    if var rootPath = path.root as? Path {
      var (root, path) = rootPath.accept(pathVisitor: &self)
      path.appendComponent(pathComponent)
      return (root, path)
    } else {
      var root = path.root
      let rootInstruction = root.accept(&self)
      let path = NestedPath([pathComponent])
      return (op: rootInstruction, path: path)
    }
  }

  public mutating func visit(path: inout PropPath) -> PathResult {
    let pathComponent = NestedPath.Component.name(path.name)

    if var rootPath = path.root as? Path {
      var (root, path) = rootPath.accept(pathVisitor: &self)
      path.appendComponent(pathComponent)
      return (root, path)
    } else {
      var root = path.root
      let rootInstruction = root.accept(&self)
      let path = NestedPath([pathComponent])
      return (op: rootInstruction, path: path)
    }
  }
}

private var gradientFunctionCache: [Function: Function] = [:]

/// Returns the functional gradient of `function`.
public func gradient(_ function: Function) -> Function {
  // Look up gradient function in cache.
  if let gradientFunction = gradientFunctionCache[function] {
    return gradientFunction
  }

  // Perform preliminary optimizations on original function that are important for AD.
  _ = PathFolding.run(on: function)

  let name = "\(function.name)_grad"
  // Compute the gradient function return type.
  let argumentCount = function.arguments.count
  let results = function.arguments.map { arg in
    StructProp(mutability: .let, name: "d\(arg.printedName)", type: arg.type)
  }
  let returnType: Type
  let returnTypeName = "Pair\(argumentCount)"
  if argumentCount == 1 {
    returnType = function.arguments[0].type
  } else {
    returnType = .struct(name: returnTypeName, props: results)
  }
  // Create the gradient function by cloning the original function.
  let gradient = function.makeClone(named: name)
  let originalResult = gradient.result
  gradient.returnType = returnType
  gradient.instructions.removeLast()

  let builder = IRBuilder(function: gradient)

  var adjointMapping: [Use: Use] = [:]

  func getOrCreateAdjoint(_ x: Use) -> Use {
    assert(
      x.parent == nil || x.parent == gradient,
      "Primal \(x) must be defined in gradient function")
    if let adjoint = adjointMapping[x] {
      return adjoint
    }
    let adjoint = %builder.buildInstruction(.alloc(x.type))
    adjointMapping[x] = adjoint
    return adjoint
  }

  func setAdjoint(_ x: Use, adjoint: Use) {
    assert(
      x.parent == nil || x.parent == gradient,
      "Primal \(x) must be defined in gradient function")
    assert(
      adjoint.parent == nil || adjoint.parent == gradient,
      "Adjoint \(adjoint) must be defined in gradient function")
    adjointMapping[x] = adjoint
  }

  /// Accumulates `rhs` into `lhs`.
  func accumulate(into lhs: Use, _ rhs: Use) {
    builder.buildInstruction(.accumulate(lhsBase: lhs, lhsPath: NestedPath(), rhs: rhs))
  }

  /// Accumulates `rhs` into `lhs` accessed with `lhsPath`.
  func accumulate(into lhs: Use, at lhsPath: NestedPath, _ rhs: Use) {
    builder.buildInstruction(.accumulate(lhsBase: lhs, lhsPath: lhsPath, rhs: rhs))
  }

  // Set seed.
  setAdjoint(originalResult, adjoint: %builder.buildInstruction(.float(1)))

  for i in gradient.instructions.indices.reversed() {
    let inst = gradient.instructions[i]
    print("Gradient transform for \(function.name): visiting instruction \(inst)")
    switch inst.kind {
    // Some instructions have no adjoints: integer/float literals, allocations, return.
    case .int, .float, .alloc, .`let`, .return:
      break

    // Handle binary operations.
    case .binaryOp:
      break

    // Handle numeric binary instructions.
    case let .numericBinary(op, lhs, rhs):
      let instAdjoint = getOrCreateAdjoint(%inst)
      let lhsAdjoint = getOrCreateAdjoint(lhs)
      let rhsAdjoint = getOrCreateAdjoint(rhs)

      switch op {
      // Original:
      //   z = x + y
      // Adjoint:
      //   ∂x += ∂z
      //   ∂y += ∂z
      case .add:
        accumulate(into: lhsAdjoint, instAdjoint)
        accumulate(into: rhsAdjoint, instAdjoint)

      // Original:
      //   z = x - y
      // Adjoint:
      //   ∂x += ∂z
      //   ∂y -= ∂z
      case .sub:
        accumulate(into: lhsAdjoint, instAdjoint)
        let negOne = %builder.buildInstruction(.float(-1))
        let negInstAdjoint = %builder.buildInstruction(
          .numericBinary(.mul, negOne, instAdjoint))
        accumulate(into: rhsAdjoint, negInstAdjoint)

      // Original:
      //   z = x * y
      // Adjoint:
      //   ∂x += y * ∂z
      //   ∂y += x * ∂z
      case .mul:
        let lhsInstAdjoint = %builder.buildInstruction(
          .numericBinary(.mul, rhs, instAdjoint))
        let rhsInstAdjoint = %builder.buildInstruction(
          .numericBinary(.mul, lhs, instAdjoint))
        accumulate(into: lhsAdjoint, lhsInstAdjoint)
        accumulate(into: rhsAdjoint, rhsInstAdjoint)

      // Original:
      //   z = x / y
      // Adjoint:
      //   ∂x += ∂z / y
      //   ∂y += -x / y * y * ∂z
      case .div:
        let lhsInstAdjoint = %builder.buildInstruction(
          .numericBinary(.div, instAdjoint, rhs))
        let negOne = %builder.buildInstruction(.float(-1))
        let negLhs = %builder.buildInstruction(
          .numericBinary(.mul, negOne, lhs))
        let rhsSquared = %builder.buildInstruction(
          .numericBinary(.mul, rhs, rhs))
        let rhsInstAdjointDenominator = %builder.buildInstruction(
          .numericBinary(.mul, rhsSquared, instAdjoint))
        let rhsInstAdjoint = %builder.buildInstruction(
          .numericBinary(.mul, negLhs, rhsInstAdjointDenominator))
        accumulate(into: lhsAdjoint, lhsInstAdjoint)
        accumulate(into: rhsAdjoint, rhsInstAdjoint)
      }

    // Handle path accesses.
    // Original:
    //   y = base.x
    // Adjoint:
    //   ∂base.x += ∂y
    case let .path(base: base, path: path):
      let instAdjoint = getOrCreateAdjoint(%inst)
      let baseAdjoint = getOrCreateAdjoint(base)
      accumulate(into: baseAdjoint, at: path, instAdjoint)

    // Handle function calls.
    // Original:
    //   y = f(x0, x1, x2, ...)
    // Adjoint:
    //   (∂x0, ∂x1, ∂x2, ...) += f_pullback(y)
    case let .call(callee, arguments):
      guard callee.isFunctionReference else {
        fatalError("Callee must be a function reference")
      }

      let instAdjoint = getOrCreateAdjoint(%inst)
      if case let .intrinsic(op) = callee {
        guard arguments.count == 2 else {
          fatalError("Expected binary operation to have two arguments")
        }

        let lhs = arguments[0]
        let rhs = arguments[1]
        let lhsAdjoint = getOrCreateAdjoint(lhs)
        let rhsAdjoint = getOrCreateAdjoint(rhs)

        switch op {
        case .eq, .ne, .lt, .le, .ge, .gt:
          break

        // Original:
        //   z = x + y
        // Adjoint:
        //   ∂x += ∂z
        //   ∂y += ∂z
        case .add:
          accumulate(into: lhsAdjoint, instAdjoint)
          accumulate(into: rhsAdjoint, instAdjoint)

        // Original:
        //   z = x - y
        // Adjoint:
        //   ∂x += ∂z
        //   ∂y -= ∂z
        case .sub:
          accumulate(into: lhsAdjoint, instAdjoint)
          let negOne = %builder.buildInstruction(.float(-1))
          let negInstAdjoint = %builder.buildInstruction(
            .numericBinary(.mul, negOne, instAdjoint))
          accumulate(into: rhsAdjoint, negInstAdjoint)

        // Original:
        //   z = x * y
        // Adjoint:
        //   ∂x += y * ∂z
        //   ∂y += x * ∂z
        case .mul:
          let lhsInstAdjoint = %builder.buildInstruction(
            .numericBinary(.mul, rhs, instAdjoint))
          let rhsInstAdjoint = %builder.buildInstruction(
            .numericBinary(.mul, lhs, instAdjoint))
          accumulate(into: lhsAdjoint, lhsInstAdjoint)
          accumulate(into: rhsAdjoint, rhsInstAdjoint)

        // Original:
        //   z = x / y
        // Adjoint:
        //   ∂x += ∂z / y
        //   ∂y += -x / y * y * ∂z
        case .div:
          let lhsInstAdjoint = %builder.buildInstruction(
            .numericBinary(.div, instAdjoint, rhs))
          let negOne = %builder.buildInstruction(.float(-1))
          let negLhs = %builder.buildInstruction(
            .numericBinary(.mul, negOne, lhs))
          let rhsSquared = %builder.buildInstruction(
            .numericBinary(.mul, rhs, rhs))
          let rhsInstAdjointDenominator = %builder.buildInstruction(
            .numericBinary(.mul, rhsSquared, instAdjoint))
          let rhsInstAdjoint = %builder.buildInstruction(
            .numericBinary(.mul, negLhs, rhsInstAdjointDenominator))
          accumulate(into: lhsAdjoint, lhsInstAdjoint)
          accumulate(into: rhsAdjoint, rhsInstAdjoint)
        }
      } else if case let .function(f) = callee {
        _ = f
      } else {
        fatalError("Unhandled callee: \(callee)")
      }

    // Handle array construction.
    // Original:
    //   y = [x0, x1, x2, ...]
    // Adjoint:
    //   ∂x0 += ∂y[0]
    //   ∂x1 += ∂y[1]
    //   ∂x2 += ∂y[2]
    //   ...
    case .array(_):
      // TODO
      break

    // Handle struct construction.
    // Original:
    //   y = S(x0: x0, x1: x1, x2: x2, ...)
    // Adjoint:
    //   ∂x0 += (∂y).x0
    //   ∂x1 += (∂y).x1
    //   ∂x2 += (∂y).x2
    //   ...
    case .struct:
      // TODO
      break

    // Handle assignments.
    // Original:
    //   base.p = x
    // Adjoint:
    //   ∂x += (∂base).p
    case .assign:
      // TODO
      break

    // Handle accumulations.
    // Original:
    //   base.p += x
    // Adjoint:
    //   ∂x += ∂x + (∂base).p
    case .accumulate:
      // TODO
      break
    }
  }

  // Return adjoints of function parameters.
  let argumentAdjoints = gradient.arguments.map { arg in getOrCreateAdjoint(%arg) }
  let result: Use
  if argumentCount == 1 {
    assert(argumentAdjoints.count == 1)
    result = argumentAdjoints[0]
  } else {
    result = %builder.buildInstruction(
      .struct(name: returnTypeName, type: returnType, arguments: argumentAdjoints))
  }
  builder.buildInstruction(.return(result))

  // Run DCE on gradient.
  DeadCodeElimination.run(on: gradient)

  return gradient
}

extension Type {
  internal func toSign() -> Sign {
    let dummyRange: Range<String.Index> = ("".startIndex..<"".endIndex)
    switch self {
    case .unit:
      return TypeDeclRefSign(name: "Int", range: dummyRange)
    case .int:
      return TypeDeclRefSign(name: "Int", range: dummyRange)
    case .float:
      return TypeDeclRefSign(name: "Float", range: dummyRange)
    case .struct(let name, props: _):
      return TypeDeclRefSign(name: name, range: dummyRange)
    case .array(let elem, let count):
      let elemSign = elem.toSign()
      return ArraySign(base: elemSign, count: count, range: dummyRange)
    case .func(let params, let output):
      let paramSigns = params.map { $0.toSign() }
      let outputSign = output.toSign()
      return FuncSign(params: paramSigns, output: outputSign, range: dummyRange)
    case .inout(let base):
      let baseSign = base.toSign()
      return InoutSign(base: baseSign, range: dummyRange)
    case .error:
      return ErrorSign(range: dummyRange)
    }
  }
}

/// Translates lower-level SSA instructions to mvs expressions.
public struct SSAToExpressionGenerator {
  public init() {}

  /// Transforms a nullary function into an expression representing its body.
  public func visit(_ function: Function) -> Expr {
    let dummyRange: Range<String.Index> = ("".startIndex..<"".endIndex)

    var parameters: [ParamDecl] = []
    for arg in function.arguments {
      let paramDecl = ParamDecl(
        name: arg.printedName, mutability: .let, sign: arg.type.toSign(),
        range: dummyRange)
      parameters.append(paramDecl)
    }

    var environment: [Use: Expr] = [:]

    func pathToExpression(_ baseName: String, _ path: NestedPath) -> Path {
      var pathExpr: Path = NamePath(name: baseName, range: dummyRange)
      for component in path {
        switch component {
        case let .name(name):
          pathExpr = PropPath(base: pathExpr, name: name, range: dummyRange)
        case let .index(i):
          guard let indexExpr = environment[i] else {
            fatalError("Could not find index argument in environment")
          }
          pathExpr = ElemPath(base: pathExpr, index: indexExpr, range: dummyRange)
        }
      }
      return pathExpr
    }

    func zeroExpr(_ type: Type) -> Expr {
      switch type {
      case .unit, .int:
        return IntExpr(value: 0, range: dummyRange)
      case .float:
        return FloatExpr(value: 0, range: dummyRange)
      case .struct(let name, let props):
        let zeroExprs = props.map { zeroExpr($0.type) }
        return StructExpr(name: name, args: zeroExprs, range: dummyRange)
      case .array(let elem, let count):
        let zeroExprs = Array(repeating: zeroExpr(elem), count: count)
        return ArrayExpr(elems: zeroExprs, range: dummyRange)
      case .func:
        // Use a dummy zero value for functions.
        return IntExpr(value: 0, range: dummyRange)
        // fatalError("Function type \(type) cannot be converted to zero")
      case .inout:
        fatalError("Inout type \(type) cannot be converted to zero")
      case .error:
        fatalError("Error type \(type) cannot be converted to zero")
      }
    }

    var symbolCounter: Int = 0
    func gensym() -> String {
      defer { symbolCounter += 1 }
      return "x\(symbolCounter)"
    }

    func typeToSign(_ type: Type) -> Sign {
      switch type {
      case .unit:
        return TypeDeclRefSign(name: "Unit", range: dummyRange)
      case .int:
        return TypeDeclRefSign(name: "Int", range: dummyRange)
      case .float:
        return TypeDeclRefSign(name: "Float", range: dummyRange)
      case .struct(name: let name, props: _):
        return TypeDeclRefSign(name: name, range: dummyRange)
      case .array(elem: let elem, count: let count):
        let elemSign = typeToSign(elem)
        return ArraySign(base: elemSign, count: count, range: dummyRange)
      case .func(params: let params, output: let output):
        let paramSigns = params.map { $0.toSign() }
        let outputSign = output.toSign()
        return FuncSign(params: paramSigns, output: outputSign, range: dummyRange)
      case .inout(base: let base):
        return InoutSign(base: typeToSign(base), range: dummyRange)
      case .error:
        return ErrorSign(range: dummyRange)
      }
    }

    func generateExpression(_ index: Int) -> Expr {
      let instruction = function.instructions[index]
      print("generateExpression index \(index): \(instruction)")
      if index == function.instructions.count - 1 {
        assert(instruction.kind.isReturn, "Expected last instruction to be a return")
        guard case let .return(value) = instruction.kind else {
          fatalError("Badness")
        }
        return NamePath(name: "p", range: dummyRange)
      } else {
        let name = instruction.name ?? gensym()

        // Create a binding expression whose body is in the next instruction
        // in the function.
        let body = generateExpression(index + 1)

        // Compute the initializer of the binding expression.
        let initializer: Expr
        switch instruction.kind {
        case let .int(i):
          initializer = IntExpr(value: i, range: dummyRange)
        case let .float(f):
          initializer = FloatExpr(value: f, range: dummyRange)
          environment[%instruction] = initializer
          print("Visiting float expression")
          print(environment)
        case .binaryOp:
          fatalError("Binary op not yet supported")
        case let .numericBinary(op, lhs, rhs):
          guard let lhsExpr = environment[lhs] else {
            fatalError("Could not find \(lhs) in environment: \(environment)")
          }
          guard let rhsExpr = environment[rhs] else {
            fatalError("Could not find \(rhs) in environment: \(environment)")
          }
          let operExpr = OperExpr(kind: op.binaryOp, range: dummyRange)
          initializer = InfixExpr(lhs: lhsExpr, rhs: rhsExpr, oper: operExpr, range: dummyRange)
        case let .path(base: base, path: path):
          return pathToExpression(base.identifier, path)
        case let .call(callee, arguments):
          fatalError()
        // case let .array(elements):
        case .array(_):
          fatalError()
        case .struct(name: let name, type: let type, arguments: let arguments):
          fatalError()
        case .assign(lhsBase: let lhsBase, lhsPath: let lhsPath, rhs: let rhs):
          let lhsPath = pathToExpression(lhsBase.identifier, lhsPath)
          print("environment: \(environment)")
          guard let rhsExpr = environment[rhs] else {
            fatalError("Could not find \(rhs) in environment: \(environment)")
          }
          let assignExpr = AssignExpr(
            lvalue: lhsPath, rvalue: rhsExpr, body: body, range: dummyRange)
          return assignExpr
        case .accumulate(lhsBase: let lhsBase, lhsPath: let lhsPath, rhs: let rhs):
          fatalError()
        case let .alloc(type):
          initializer = zeroExpr(type)
        case let .`let`(type, value):
          /*
          let bindingDecl = BindingDecl(
            mutability: .let, name: name, sign: typeToSign(instruction.type),
            range: dummyRange)
          let bindingExpr = BindingExpr(
            decl: bindingDecl, initializer: value, body: body,
            range: dummyRange)
          return bindingExpr
          */
          fatalError()
        case .return(_):
          fatalError()
        }
        environment[%instruction] = initializer

        let bindingDecl = BindingDecl(
          mutability: .let, name: name, sign: typeToSign(instruction.type),
          range: dummyRange)
        let bindingExpr = BindingExpr(
          decl: bindingDecl, initializer: initializer, body: body,
          range: dummyRange)
        return bindingExpr
      }
    }

    let finalExpression = generateExpression(0)
    return finalExpression
  }
}

/*
/// Translates lower-level SSA instructions to mvs expressions.
public struct SSAToExpressionGenerator {
  public init() {}

  /// Transforms a nullary function into an expression representing its body.
  public func visit(_ function: Function) -> Expr {
    let dummyRange: Range<String.Index> = ("".startIndex..<"".endIndex)

    var environment: [String: Expr] = [:]
    var bindings: [String: BindingExpr] = [:]

    var binding: BindingExpr? = nil
    var lastBadBodyExpressionName: String? = nil

    // The expression built from visiting SSA instructions.
    var lastExpression: Expr? = nil

    var parameters: [ParamDecl] = []
    for arg in function.arguments {
      let paramDecl = ParamDecl(
        name: arg.printedName, mutability: .let, sign: arg.type.toSign(),
        range: dummyRange)
      parameters.append(paramDecl)
    }

    var functionBodyExpr: Expr? = nil

    func pathToExpression(_ baseName: String, _ path: NestedPath) -> Path {
      var pathExpr: Path = NamePath(name: baseName, range: dummyRange)
      for component in path {
        switch component {
        case let .name(name):
          pathExpr = PropPath(base: pathExpr, name: name, range: dummyRange)
        case let .index(i):
          guard let indexExpr = environment[i.identifier] else {
            fatalError("Could not find index argument in environment")
          }
          pathExpr = ElemPath(base: pathExpr, index: indexExpr, range: dummyRange)
        }
      }
      return pathExpr
    }

    func zeroExpr(_ type: Type) -> Expr {
      switch type {
      case .unit, .int:
        return IntExpr(value: 0, range: dummyRange)
      case .float:
        return FloatExpr(value: 0, range: dummyRange)
      case .struct(let name, let props):
        let zeroExprs = props.map { zeroExpr($0.type) }
        return StructExpr(name: name, args: zeroExprs, range: dummyRange)
      case .array(let elem, let count):
        let zeroExprs = Array(repeating: zeroExpr(elem), count: count)
        return ArrayExpr(elems: zeroExprs, range: dummyRange)
      case .func:
        // Use a dummy zero value for functions.
        return IntExpr(value: 0, range: dummyRange)
        // fatalError("Function type \(type) cannot be converted to zero")
      case .inout:
        fatalError("Inout type \(type) cannot be converted to zero")
      case .error:
        fatalError("Error type \(type) cannot be converted to zero")
      }
    }

    func generateExpression(_ index: Int) -> Expr {

    }

    // let returnType = function.returnType.toSign()
    for inst in function.instructions {
      let name = (%inst).identifier

      switch inst.kind {
      case let .int(i):
        let intExpr = IntExpr(value: i, range: dummyRange)
        environment[name] = intExpr
      case let .float(f):
        let floatExpr = FloatExpr(value: f, range: dummyRange)
        environment[name] = floatExpr
      case let .binaryOp(op, argumentType: _):
        let opExpr = OperExpr(kind: op, range: dummyRange)
        environment[name] = opExpr
      case .numericBinary:
        fatalError("Handle numeric binary operations")
      case .path(let base, let path):
        let pathName = base.identifier
        var pathExpr: Expr = NamePath(name: pathName, range: dummyRange)
        for component in path {
          switch component {
          case let .name(name):
            pathExpr = PropPath(base: pathExpr, name: name, range: dummyRange)
          case let .index(i):
            guard let indexExpr = environment[i.identifier] else {
              fatalError("Could not find index argument in environment: \(i.identifier)")
            }
            pathExpr = ElemPath(base: pathExpr, index: indexExpr, range: dummyRange)
          }
        }
        environment[pathName] = pathExpr
      case let .call(callee, arguments):
        guard case let .function(f) = callee else {
          fatalError("Callee must be a function")
        }
        let functionExpr = visit(f)
        let argumentExprs = arguments.map { arg -> Expr in
          guard let expr = environment[arg.identifier] else {
            fatalError("Could not find argument in environment: \(arg.identifier)")
          }
          return expr
        }
        let callExpr = CallExpr(callee: functionExpr, args: argumentExprs, range: dummyRange)
        environment[name] = callExpr
      case let .array(elements):
        let elementExprs = elements.map { element -> Expr in
          guard let expr = environment[element.identifier] else {
            fatalError("Could not find element in environment: \(element.identifier)")
          }
          return expr
        }
        let arrayExpr = ArrayExpr(elems: elementExprs, range: dummyRange)
        environment[name] = arrayExpr
      case let .struct(structName, _, arguments):
        let argumentExprs = arguments.map { arg -> Expr in
          guard let expr = environment[arg.identifier] else {
            fatalError("Could not find argument in environment: \(arg.identifier)")
          }
          return expr
        }
        let structExpr = StructExpr(name: structName, args: argumentExprs, range: dummyRange)
        print("HELLO STRUCT EXPR NAME: \(name)")
        environment[name] = structExpr
      case let .assign(lhsBase, lhsPath, rhs):
        let lhsPath = pathToExpression(lhsBase.identifier, lhsPath)
        guard let rhsExpr = environment[rhs.identifier] else {
          print(environment)
          fatalError("Could not find rhs in environment: \(rhs.identifier)")
        }
        print("Found assign instruction:")
        let dummyExpr = IntExpr(value: 1337, range: dummyRange)
        // TODO: replace `dummyExpr` with a real body representing the rest of the program.
        let assignExpr = AssignExpr(
          lvalue: lhsPath, rvalue: rhsExpr, body: dummyExpr, range: dummyRange)
        environment[name] = assignExpr
      case let .accumulate(lhsBase, lhsPath, rhs):
        let lhsPath = pathToExpression(lhsBase.identifier, lhsPath)
        // TODO: Either (1) look up rhs in environment or (2) create a path expression?
        guard let rhsExpr = environment[rhs.identifier] else {
          fatalError("Could not find rhs in environment")
        }
        let dummyExpr = IntExpr(value: 1337, range: dummyRange)
        // TODO: replace AssignExpr with a real accumulation operation.
        // TODO: need to sequence accumulation into other expressions to make sure they happen.
        // TODO: replace `dummyExpr` with a real body representing the rest of the program.
        let assignExpr = AssignExpr(
          lvalue: lhsPath, rvalue: rhsExpr, body: dummyExpr, range: dummyRange)
        environment[name] = assignExpr
      case let .alloc(type):
        let zeroExpr = zeroExpr(type)
        /*
        let dummyExpr = IntExpr(value: 1, range: dummyRange)
        // TODO: replace AssignExpr with a real accumulation operation.
        // TODO: need to sequence accumulation into other expressions to make sure they happen.
        // TODO: replace `dummyExpr` with a real body representing the rest of the program.
        let assignExpr = AssignExpr(lvalue: NamePath(name: name, range: dummyRange), rvalue: zeroExpr, body: dummyExpr, range: dummyRange)
        */
        print("alloc name: \(name), \(String(describing: inst.name))")
        // environment[name] = zeroExpr
        /*
        let bindingDecl = BindingDecl(
          mutability: .var, name: name,
          sign: TypeDeclRefSign(name: "Float", range: dummyRange),
          range: dummyRange)
        let bindingExpr = BindingExpr(
          decl: bindingDecl, initializer: expr, body: dummyExpr,
          range: dummyRange)
        */
        environment[name] = bindingExpr
      case let .return(value):
        print("Return found: \(value.identifier)")
        guard let returnExpr = environment[value.identifier] else {
          fatalError("Returned expression must be defined")
        }
        _ = returnExpr
        let returnPath = NamePath(name: value.identifier, range: dummyRange)
        // functionBodyExpr = returnExpr
        functionBodyExpr = returnPath
      }

      print("Assigned name: \(name), \(inst)")

      guard let expr = environment[name] else {
        fatalError("Converted expression must be defined: \(name)")
      }

      // Check if last binding is defined. If so, update its dummy body.
      if binding != nil {
        print("`binding` is defined! Setting body to \(expr)")
        binding?.body = expr
        binding = nil
      }
      print("NAME '\(name)' vs REAL NAME '\(inst.name as Any)'")

      // Check instructions with explicit names.
      // These represent bindings in the original expression.
      if let realName = inst.name {
        print("Instruction real name: \(realName), will create dummy binding")
        let dummyExpr = IntExpr(value: 1337, range: dummyRange)
        // TODO: Choose between let and var mutability.
        // Note: do not hardcode Float type.
        let bindingDecl = BindingDecl(
          mutability: .var, name: name,
          sign: TypeDeclRefSign(name: "Float", range: dummyRange),
          range: dummyRange)
        let bindingExpr = BindingExpr(
          decl: bindingDecl, initializer: expr, body: dummyExpr,
          range: dummyRange)
        // /*
        environment[name] = bindingExpr
        environment[realName] = bindingExpr
        // */
        // NOTE: unused
        bindings[realName] = bindingExpr
        binding = bindingExpr
      }

      // Check if last binding is defined. If so, update its dummy body.
      if let expressionName = lastBadBodyExpressionName {
        guard let lastBadBodyExpression = environment[expressionName] else {
          fatalError("Expression not found in environment")
        }
        print("Found last bad body expression name: \(expressionName)")
        print("Found last bad body expression: \(lastBadBodyExpression)")
        if var assignExpr = lastBadBodyExpression as? AssignExpr {
          assignExpr.body = expr
          environment[expressionName] = assignExpr
          lastExpression = assignExpr
        } else if var bindingExpr = lastBadBodyExpression as? BindingExpr {
          bindingExpr.body = expr
          environment[expressionName] = bindingExpr
          lastExpression = bindingExpr
        } else {
          fatalError("Unhandled case: \(lastBadBodyExpression)")
        }
        lastBadBodyExpressionName = nil
      }

      switch inst.kind {
      case .assign, .accumulate, .alloc:
        lastBadBodyExpressionName = name
      default:
        break
      }
    }

    guard let lastExpression = lastExpression else {
      fatalError("We failed to find the expression")
    }
    _ = lastExpression

    guard let functionBodyExpr = functionBodyExpr else {
      fatalError("Function body expression must be defined if return instruction is visited")
    }

    /*
    // Check if last binding is defined. If so, update its dummy body.
    if let expressionName = lastBadBodyExpressionName {
      guard let lastBadBodyExpression = environment[expressionName] else {
        fatalError("Expression not found in environment")
      }
      if var assignExpr = lastBadBodyExpression as? AssignExpr {
        assignExpr.body = functionBodyExpr
        environment[expressionName] = assignExpr
      } else if var bindingExpr = lastBadBodyExpression as? BindingExpr {
        bindingExpr.body = functionBodyExpr
        environment[expressionName] = bindingExpr
      } else {
        fatalError("Unhandled case")
      }
      lastBadBodyExpressionName = nil
    }
    */

    print("Final environment:")
    print(environment)

    print("Final bindings:")
    print(bindings)

    /*
    let functionExpr = FuncExpr(
      params: parameters, output: returnType, body: functionBodyExpr, range: dummyRange)
    return functionExpr
    */
    return functionBodyExpr
    // return lastExpression
  }
}
*/
