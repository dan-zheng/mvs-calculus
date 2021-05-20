public struct DataFlowGraph {
  fileprivate var users: [Use: Set<Instruction>] = [:]
}

extension DataFlowGraph {
  fileprivate mutating func insert(_ user: Instruction, for def: Use) {
    if !users.keys.contains(def) {
      users[def] = []
    }
    users[def]!.insert(user)
  }
}

extension DataFlowGraph {
  /// Returns a set of users.
  public func successors(of def: Use) -> Set<Instruction> {
    return users[def] ?? []
  }

  /// Returns a set of users.
  public func successors(of inst: Instruction) -> Set<Instruction> {
    return successors(of: .instruction(inst))
  }

  /// Returns a set of users within the basic block.
  public func successors(of def: Use, in function: Function) -> Set<Instruction> {
    var users = successors(of: def)
    for user in users where user.parent != function {
      users.remove(user)
    }
    return users
  }

  /// Predecessors.
  public func predecessors(of inst: Instruction) -> [Use] {
    return inst.operands
  }
}

/// Analyzes function and produces a graph from definitions to users.
public struct DataFlowGraphAnalysis: AnalysisPass {
  public typealias Body = Function
  public typealias Result = DataFlowGraph

  public static func run(on body: Function) -> DataFlowGraph {
    var userGraph = DataFlowGraph()
    for inst in body.instructions {
      for use in inst.operands {
        userGraph.insert(inst, for: use)
      }
    }
    return userGraph
  }
}
