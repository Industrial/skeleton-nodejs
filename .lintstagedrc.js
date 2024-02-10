export default {
  '*.{js,jsx,ts,tsx}': [
    'eslint --fix',
    'pnpm test:staged',
    'pnpm test:staged:coverage',
  ],
}