import { Ollama, OllamaEmbeddings } from '@langchain/ollama'

export const createOllamaEmbeddingsModel = (
  host: string,
  port: number,
  modelName: string,
): OllamaEmbeddings =>
  new OllamaEmbeddings({
    baseUrl: `http://${host}:${port}`,
    model: modelName,
  })

export const createOllamaRetrievalModel = (
  host: string,
  port: number,
  modelName: string,
): Ollama =>
  new Ollama({
    baseUrl: `http://${host}:${port}`,
    model: modelName,
  })
