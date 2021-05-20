/// Dead code elimination (traditional algorithm)
public struct DeadCodeElimination: TransformPass {
  public typealias Body = Function

  @discardableResult
  public static func run(on body: Function) -> Bool {
    var changed = false
    var workList: [Instruction] = []
    var count = 0
    /// Iterate over the original function, only adding instructions to the
    /// worklist if they actually need to be revisited. This avoids having
    /// to pre-init the worklist with the entire function's worth of
    /// instructions.
    for inst in body.instructions where !workList.contains(inst) {
      changed =
        performDCE(
          on: inst,
          workList: &workList,
          count: &count) || changed
    }
    while let inst = workList.popLast() {
      changed =
        performDCE(
          on: inst,
          workList: &workList,
          count: &count) || changed
    }
    return changed
  }

  private static func performDCE(
    on inst: Instruction,
    workList: inout [Instruction],
    count: inout Int
  ) -> Bool {
    let function = inst.parent
    var dfg = DataFlowGraphAnalysis.run(on: function)
    let sideEffectInfo = SideEffectAnalysis.run(on: function)

    /// If instruction is not trivially dead, change nothing.
    guard dfg.successors(of: inst).isEmpty,
      sideEffectInfo[inst] == .none,
      !inst.kind.isTerminator
    else { return false }
    /// Eliminate
    print("About to remove instruction: \(inst)")
    print("Successors:", dfg.successors(of: inst))
    print(function)
    inst.removeFromParent()
    count += 1
    /// Remove instruction and check users
    /// Get new user analysis
    dfg = DataFlowGraphAnalysis.run(on: function)
    /// For original uses, check if they need to be revisited
    for case let .instruction(usee) in inst.operands
    where dfg.successors(of: usee).isEmpty
      && sideEffectInfo[usee] == .none
      && !inst.kind.isTerminator
    {
      workList.append(usee)
    }
    return true
  }
}
