export default {
  '{applications,packages}/**/*': [
    `nx format:write --uncommitted`,
    `nx affected --uncommitted --target typecheck`,
    `nx affected --uncommitted --target lint`,
    files => `git add ${files.join(' ')}`,
  ],
};
