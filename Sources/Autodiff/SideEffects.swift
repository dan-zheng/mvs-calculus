import Foundation

public struct SideEffectProperties: OptionSet {
  public let rawValue: Int
  public static let mayWriteToMemory = SideEffectProperties(rawValue: 1 << 0)
  public static let none: SideEffectProperties = []
  public static let all: SideEffectProperties = [.mayWriteToMemory]

  public init(rawValue: Int) {
    self.rawValue = rawValue
  }
}

public struct SideEffectInfo {
  private var table: [Function: SideEffectProperties] = [:]

  public subscript(body: Function) -> SideEffectProperties {
    get {
      guard let props = table[body] else {
        fatalError("No SideEffectProperties for function \(body.name)")
      }
      return props
    }
    set { table[body] = newValue }
  }

  public subscript(instruction: Instruction) -> SideEffectProperties {
    switch instruction.kind {
    case _ where instruction.kind.mustWriteToMemory:
      return .mayWriteToMemory
    case .call(.function(let callee), _):
      return self[callee]
    default:
      return []
    }
  }
}

/// Conservatively analyzes side effects of all functions in the module
public struct SideEffectAnalysis: AnalysisPass {
  public typealias Body = Function
  public typealias Result = SideEffectInfo

  public static func run(on body: Function) -> SideEffectInfo {
    var result = SideEffectInfo()
    var sameModuleCalls: [(Function, Function)] = []

    var functions = Set([body])
    func collectCallees(_ function: Function) {
      for inst in function.instructions {
        guard case let .call(.function(callee), _) = inst.kind else {
          continue
        }
        if !functions.contains(callee) {
          collectCallees(callee)
        }
        functions.insert(callee)
      }
    }

    // Find instructions that definitely have side-effects, and collect
    // callers and callees of functions.
    for function in functions {
      var props: SideEffectProperties = []
      for inst in function.instructions {
        // Check instructions that definitely have side effects.
        if inst.kind.mustWriteToMemory {
          props.insert(.mayWriteToMemory)
        }
        // Check function calls.
        switch inst.kind {
        case .call(.function(let callee), _):
          // Call within the same module.
          sameModuleCalls.append((function, callee))
        case .call:
          // External call: be conservative.
          props = .all
        default:
          break
        }
      }
      result[function] = props
    }

    // For each function call, union caller's properties with callee's.
    var propChanged = false
    repeat {
      for (caller, callee) in sameModuleCalls
      where result[caller] != result[callee] {
        result[caller].formUnion(result[callee])
        propChanged = true
      }
    } while propChanged

    return result
  }
}
