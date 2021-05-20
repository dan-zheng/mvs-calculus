import Foundation
import ArgumentParser

import AST
import Autodiff
import CodeGen
import Parse
import Sema

struct MVS: ParsableCommand {

  @ArgumentParser.Argument(help: "The source program.", transform: URL.init(fileURLWithPath:))
  var inputFile: URL

  @Option(help: "Wrap the program inside a benchmark.")
  var benchmark: Int?

  @Flag(help: "Disable the printing of the program's value.")
  var noPrint: Bool = false

  func run() throws {
    let input = try String(contentsOf: inputFile)

    // Create a diagnostic consumer.
    let console = Console(source: input)

    // Parse the program.
    var parser = MVSParser()
    guard var program = parser.parse(source: input, diagConsumer: console) else { return }

    // Type check the program.
    var checker = TypeChecker(diagConsumer: console)
    guard checker.visit(&program) else { return }

    // Emit the program's IR.
    let mode: EmitterMode
    if let n = benchmark {
      mode = .benchmark(count: n)
    } else {
      mode = .release
    }

    var emitter = try Emitter(mode: mode, shouldEmitPrint: !noPrint)
    let module = try emitter.emit(program: &program)
    module.dump()

    /*
    do {
      print("LLVM module:")
      let llvmModule = try emitter.emit(program: &program)
      llvmModule.dump()

      print("LLVM module (optimized):")
      let llvmModuleOptimized = try emitter.emit(program: &program, optimize: true)
      llvmModuleOptimized.dump()
    }
    */

    // Convert the program to SSA.
    print("Transforming expression to SSA:")
    var ssaGenerator = InstructionGenerator()
    ssaGenerator.visit(&program)
    let ssaModule = ssaGenerator.module
    print(ssaModule)

    /*
    let generator = SSAToExpressionGenerator()
    let expr = generator.visit(ssaModule.findFunction("main")!)
    print("Transformed expression:")
    print(expr)

    var transformedProgram = Program(types: [], entry: expr)
    guard checker.visit(&transformedProgram) else {
      fatalError("Could not type-check transformed program")
    }
    do {
      print("Transformed LLVM module:")
      let llvmModule = try emitter.emit(program: &transformedProgram)
      llvmModule.dump()

      /*
      print("Transformed LLVM module (optimized):")
      let llvmModuleOptimized = try emitter.emit(program: &transformedProgram, optimize: true)
      llvmModuleOptimized.dump()
      */
    }
    */
  }

}

MVS.main()
