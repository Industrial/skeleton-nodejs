import ts from 'typescript'
import { type Func, type Local, func, i64, Module, ValueType } from 'wasmati'

export type OutputFunction = Func<ReadonlyArray<ValueType>, ReadonlyArray<ValueType>>

const getKind = (node: ts.Node) => {
  return ts.SyntaxKind[node.kind]
}

const handleBinaryExpression = (node: ts.BinaryExpression, leftValue: Local<'i64'>, rightValue: Local<'i64'>) => {
  const { operatorToken } = node
  switch (operatorToken.kind) {
  case ts.SyntaxKind.PlusToken:
    return i64.add(leftValue, rightValue)
  case ts.SyntaxKind.MinusToken:
    return i64.sub(leftValue, rightValue)
  case ts.SyntaxKind.AsteriskToken:
    return i64.mul(leftValue, rightValue)
  case ts.SyntaxKind.SlashToken:
    return i64.div_s(leftValue, rightValue)
  default:
    return undefined
  }
}

const handleArrowFunction = (node: ts.ArrowFunction): [string, OutputFunction] => {
  const name = node.parent.getChildren()[0].getText()
  const wasmFunc = func({
    in: [i64, i64],
    locals: [],
    out: [i64],
  }, ([leftValue, rightValue]) => {
    if (!ts.isBinaryExpression(node.body)) {
      throw new Error('Expected binary expression')
    }
    handleBinaryExpression(node.body, leftValue, rightValue)
  })
  return [name, wasmFunc]
}

const handleFunctionDeclaration = (node: ts.FunctionDeclaration): [string, OutputFunction] => {
  if (!node.name) {
    throw new Error('Expected function name')
  }
  const name = node.name.getText()
  const block = node.getChildren().find((child) => {
    return getKind(child) === 'Block'
  })
  if (!block) {
    throw new Error('Expected block')
  }
  const expression = block.getChildAt(1).getChildren()[0].getChildAt(1)
  if (!ts.isBinaryExpression(expression)) {
    throw new Error('Expected binary expression')
  }
  const wasmFunc = func({
    in: [i64, i64],
    out: [i64],
  }, ([leftValue, rightValue]) => {
    handleBinaryExpression(expression, leftValue, rightValue)
  })
  return [name, wasmFunc]
}

const visitRootNode = (nodes: Array<[string, OutputFunction]>) => {
  return (_context: ts.TransformationContext) => {
    return (node: ts.SourceFile): ts.Node => {
      const visitNodes = (statements: Array<ts.Node>) => {
        for (const statement of statements) {
          const kind = getKind(statement)
          switch (kind) {
          case 'FirstStatement':
          case 'SyntaxList':
          case 'VariableDeclarationList':
          case 'VariableDeclaration':
            visitNodes(statement.getChildren())
            break
          case 'ArrowFunction':
            nodes.push(handleArrowFunction(statement as ts.ArrowFunction))
            break
          case 'FunctionDeclaration':
            nodes.push(handleFunctionDeclaration(statement as ts.FunctionDeclaration))
            break
          case 'ConstKeyword':
          default:
            break
          }
        }
      }
      visitNodes([...node.statements])
      return node
    }
  }
}

const compile = (sourceFile: ts.SourceFile) => {
  const nodes: Array<[string, OutputFunction]> = []
  const result = ts.transform(sourceFile, [visitRootNode(nodes)])
  result.dispose()
  const exports = Array.from(nodes).reduce((acc, [key, value]) => {
    return {
      ...acc,
      [key]: value,
    }
  }, {})
  return Module({ exports })
}

const sourceCode = `
export const add1 = (x: number, y: number): number => x + y;

export function add(x: number, y: number): number {
  return x + y;
}

export function subtract(x: number, y: number): number {
  return x - y;
}

export function multiply(x: number, y: number): number {
  return x * y;
}

export function divide(x: number, y: number): number {
  return x / y;
}
`

const sourceFile = ts.createSourceFile('source.ts', sourceCode, ts.ScriptTarget.Latest, true, ts.ScriptKind.TS)
const wasmModule = compile(sourceFile)
const mod = await wasmModule.instantiate()
const add = mod.instance.exports.add as (x: bigint, y: bigint) => bigint
console.log(add(1n, 1n))

