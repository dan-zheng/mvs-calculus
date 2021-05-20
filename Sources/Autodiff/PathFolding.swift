/// Path folding transform.
///
/// Example:
///
///   struct ArrayPair {
///     var x: [Float,3]
///     var y: [Float,3]
///   }
///
///   Before PathFolding:
///   func @nestedIndex(p: ArrayPair) -> Float {
///     %0 = 2: Int
///     %1 = %p.x
///     %x = %1[%0: Int]
///     return %x: Float
///   }
///
///   After PathFolding:
///   func @nestedIndex(p: ArrayPair) -> Float {
///     %0 = 2: Int
///     %1 = %p.x[%0: Int]
///     return %1: Float
///   }
public struct PathFolding: TransformPass {
  public typealias Body = Function

  public static func run(on body: Function) -> Bool {
    var changed = false
    let dfg = DataFlowGraphAnalysis.run(on: body)
    /// Iterate over the original function, only adding instructions to the
    /// worklist if they actually need to be revisited. This avoids having
    /// to pre-init the worklist with the entire function's worth of
    /// instructions.
    for inst in body.instructions {
      // Process only path instructions.
      guard case let .path(base: rootBase, path: rootPath) = inst.kind else {
        continue
      }
      // Check all path instruction successors.
      let successors = dfg.successors(of: inst)
      func isPathInstruction(_ inst: Instruction) -> Bool {
        if case .path = inst.kind {
          return true
        }
        return false
      }
      if !successors.allSatisfy(isPathInstruction) {
        continue
      }
      // Replace each successor with a new path instruction containing the root
      // path.
      for successor in successors {
        let builder = IRBuilder(function: body)
        builder.setInsertionPoint(before: successor)

        guard case let .path(base: base, path: path) = successor.kind else {
          continue
        }
        assert(base == %inst)
        let newPath = NestedPath(rootPath.components + path.components)
        let newSuccessor = builder.buildInstruction(.path(base: rootBase, path: newPath))
        successor.replaceAllUsesWith(%newSuccessor)
        successor.removeFromParent()
      }
      // Remove instruction from parent.
      inst.removeFromParent()
      changed = true
    }
    return changed
  }
}
