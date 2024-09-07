import { loadCompiled } from './lib/ts/wasm';

const wasmUrl = `${import.meta.dirname}/dist/Main.wasm`;
const jsUrl = `${import.meta.dirname}/dist/Main.js`;

export interface Exports extends WebAssembly.Exports {
  hs_init: (argc: number, argv: number) => void;
  main: () => Promise<void>;
}

const instance = await loadCompiled<Exports>(wasmUrl, jsUrl, {
  test123: (x: number) => {
    console.log('test123:env', x);
    return x + 1;
  },
});

instance.exports.hs_init(0, 0);

try {
  await instance.exports.main();
} catch (err) {
  console.error('Error during Haskell main execution:', err);
}
