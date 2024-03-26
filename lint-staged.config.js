export default {
  '{applications,packages}/**/*': [
    `nx affected --uncommitted --target lint`,
    files => `git add ${files.join(' ')}`,
  ],
};
