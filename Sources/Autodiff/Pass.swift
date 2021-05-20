public protocol Pass {
  associatedtype Body
  static var name: String { get }
}

public protocol AnalysisPass: Pass {
  associatedtype Result
  static func run(on body: Body) -> Result
}

public protocol TransformPass: Pass {
  static var shouldInvalidateAnalyses: Bool { get }
  static func run(on body: Body) -> Bool
}

extension TransformPass {
  public static var shouldInvalidateAnalyses: Bool {
    return true
  }
}

extension Pass {
  public static var name: String {
    return String(describing: self)
  }
}
