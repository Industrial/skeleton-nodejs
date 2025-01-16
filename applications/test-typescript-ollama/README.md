# Fine-tuning CodeGemma for Effect.ts

This guide explains how to fine-tune the CodeGemma model for Effect.ts-specific tasks using Ollama.

## Setup

1. Create a new model using the modelfile:
```bash
ollama create effectts-codegemma -f modelfile
```

2. Test the model:
```bash
ollama run effectts-codegemma "Refactor this code to use Effect.ts patterns: ..."
```

## Training Data Structure

The modelfile includes:
- System prompt defining the model's expertise
- Template for consistent input/output format
- Training examples covering key Effect.ts patterns:
  - Pipe pattern for function composition
  - Error handling with Effect types
  - Side effect management
  - Type safety patterns

## Best Practices

1. **Data Quality**
   - Use real-world Effect.ts code examples
   - Include both "before" and "after" code
   - Explain the reasoning behind each transformation

2. **Training Focus**
   - Effect.ts core concepts (pipe, flatMap, etc.)
   - Error handling patterns
   - Type safety considerations
   - Functional programming principles

3. **Model Usage**
   - Provide clear code context
   - Ask for specific refactoring goals
   - Review and validate suggestions

## Extending the Training

To add more training examples:
1. Create new TRAINING blocks in the modelfile
2. Focus on different Effect.ts patterns
3. Include explanations of improvements
4. Rebuild the model with new examples

## Limitations

- Fine-tuning preserves CodeGemma's general capabilities
- Model suggestions should be reviewed for correctness
- Complex refactoring may need human oversight
- Training data quality directly impacts model performance

## Additional Training Examples

Consider adding more examples for:
- Complex pipe compositions
- Error recovery patterns
- Resource management
- Concurrent operations
- Testing patterns with Effect

## Validation

After fine-tuning:
1. Test with various code samples
2. Verify type safety in suggestions
3. Check adherence to Effect.ts patterns
4. Validate error handling approaches
