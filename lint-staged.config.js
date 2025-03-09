export default {
  '{applications,packages}/**/*': [
    'nx affected --uncommitted --target nx-lint',
    (files) => `git add ${files.join(' ')}`,
  ],
}
