# Component Model
I want a way to compose component so that I only have to define the (reusable) components with TypeScript and I can combine them using a DSL (Probably JSON based).
## Concepts
### Slots
Slots tell the user of the component where to put child components. The simplest example of this is the `children: ReactNode` property. This is what builds our tree of components.
### Styling
Styling can be done in several ways. We should let the user choose their styling approach because many people have many ideas on how to do styling. The styling function should be a function that receives an object with state and returns an object with attribute names that can be applied to the component. This way, any style approach can be chosen.
#### ClassName
With CSS, CSS Modules and preprocessors like SASS/SCSS, you import the styles from a style file and the compiler/bundler will turn that into a list of class names that can be used in the component. We can make use of these existing systems so that Style files can just be written as normal and the class names they export configured for the component.
#### CSS-in-JS
The same can be done with CSS-in-JS solutions like [StyleX](https://stylexjs.com) by Meta. You write the CSS as JavaScript objects and this eventually exports a list of class names. This styling approach does conditional styling so to facilitate this, we could pass the styles a function of `(state) => styleattrs` that would return the attributes to be applied on the components. This way we separate the components from the styles but can apply the styles to the components conditionally based on state.
### Server Components
Statically rendered by default and optionally server side rendered, these components have no interaction in the browser but they may have data requirements.
### Browser Components
Whenever interaction with the user is required, we use Browser Components.
#### Behaviors
There can be a list of predefined behaviors such as Counter, Step, Toggle and more complex ones. To prevent limitations of the system, behaviors should be able to trigger events or change state. Shared state allows for interoperability between configured reusable components.
### State Management
Signals are used as a state management solution. For now, we use `@preact/signals` as it is the most complete implementation and ready for production.
### Compiler
The composition of the components should not happen at runtime but at build time. Ideally we will ship no code into the browser to achieve the JSX structure but decide it all up front in Static Rendering or on Server Side Render.
##### Toggle Behaviour
```tsx
export const config = {
  type: 'behaviour',
  name: 'toggle',
  props: {
    default: {
	  type: 'boolean',
	  optional: true,
	  default: false,
    },
  },
  returns: {
	 type: 'signal',
	 value: {
		 type: 'boolean',
	 },
  },
}
export const toggle = ({default = false}) => {
  const state = useSignal<boolean>(default)
  return state
}
```
##### Component
```tsx
export const toggleStyles = stylex.create({
  on: {
    backgroundColor: 'yellow',
    color: 'black'
  },
  off: {
    backgroundColor: 'black',
    color: 'yellow'
  }
})
export const styles = ({ toggle }) => {
  return stylex.props(toggle.value ? styles.on : styles.off)
}
export const config = {
  type: 'component',
  name: 'Lightbulb',
  props: {
    text: {
      type: 'slot',
      // TODO: Not the best name.
      value: {
	    type: 'string',
	    name: 'text',
	    optional: true,
	    default: 'Click',
      },
    },
  },
  styles,
  behaviours: ['toggle'],
}
export const Lightbulb = ({ props, styles, behaviours }) => {
  const styleAttributes = computed(() =>
    styles({
      toggle: behaviours.toggle.value
    }))
  return (
    <div {...styleAttributes.value}>{props.text.value}</div>
  )
}
```
