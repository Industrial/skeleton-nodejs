import * as runtime from 'onnxruntime-node'

async function main() {
  try {
    const session = await runtime.InferenceSession.create('./model.onnx')

    const dataA = Float32Array.from([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
    const dataB = Float32Array.from([10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120])
    const tensorA = new runtime.Tensor('float32', dataA, [3, 4])
    const tensorB = new runtime.Tensor('float32', dataB, [4, 3])

    const feeds = { a: tensorA, b: tensorB }

    const results = await session.run(feeds)

    const dataC = results.c.data
    console.log(`data of result tensor 'c': ${dataC}`)
  } catch (e) {
    console.error(`failed to inference ONNX model: ${e}.`)
  }
}

main()
