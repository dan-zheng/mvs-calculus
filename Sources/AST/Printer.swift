// /*
extension IntExpr: CustomStringConvertible {
  public var description: String {
    value.description
  }
}

extension FloatExpr: CustomStringConvertible {
  public var description: String {
    value.description
  }
}

extension ArrayExpr: CustomStringConvertible {
  public var description: String {
    "[\(elems.map { $0.description }.joined(separator: ","))]"
  }
}

extension StructExpr: CustomStringConvertible {
  public var description: String {
    "\(name)(\(args.map { $0.description }.joined(separator: ","))"
  }
}

extension Optional where Wrapped == Type {
  public var description: String {
    map { String(describing: $0) } ?? "<null>"
  }
}

extension FuncExpr: CustomStringConvertible {
  public var description: String {
    let paramsString = params.map { param in
      "\(param.name): \(param.type.description)"
    }.joined(separator: ", ")
    let outputString = output.type.description
    return "(\(paramsString)) -> \(outputString) {\n  \(body)\n}"
  }
}

extension CallExpr: CustomStringConvertible {
  public var description: String {
    "\(self.callee)(\(args.map { $0.description }.joined(separator: ","))"
  }
}

extension InfixExpr: CustomStringConvertible {
  public var description: String {
    "\(self.lhs) \(self.oper) \(self.rhs)"
  }
}

extension OperExpr.Kind: CustomStringConvertible {
  public var description: String {
    switch self {
    case .eq:
      return "=="
    case .ne:
      return "!="
    case .lt:
      return "<"
    case .le:
      return "<="
    case .ge:
      return ">="
    case .gt:
      return ">"
    case .add:
      return "+"
    case .sub:
      return "="
    case .mul:
      return "*"
    case .div:
      return "/"
    }
  }
}

extension OperExpr: CustomStringConvertible {
  public var description: String {
    "\(self.kind)"
  }
}

extension InoutExpr: CustomStringConvertible {
  public var description: String {
    "&\(path)"
  }
}

extension BindingExpr: CustomStringConvertible {
  public var description: String {
    "\(decl.mutability) \(decl.name) = \(initializer) in\n\(body)"
  }
}

extension AssignExpr: CustomStringConvertible {
  public var description: String {
    "\(lvalue) = \(rvalue) in\n\(body)"
  }
}

extension GradientExpr: CustomStringConvertible {
  public var description: String {
    "gradient"
  }
}

extension ErrorExpr: CustomStringConvertible {
  public var description: String {
    "<error>"
  }
}

extension NamePath: CustomStringConvertible {
  public var description: String {
    name
  }
}

extension PropPath: CustomStringConvertible {
  public var description: String {
    "\(base).\(name)"
  }
}

extension ElemPath: CustomStringConvertible {
  public var description: String {
    "\(base)[\(index)]"
  }
}
// */
