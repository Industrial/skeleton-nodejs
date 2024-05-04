import { computed, Signal } from '@preact/signals'
import * as stylex from '@stylexjs/stylex'
import type { JSX } from 'react'

export type BehaviourConfiguration = {
  type: 'behaviour'
  name: string
}

export type SlotConfiguration = {
  type: 'slot'
  name: string
}

export type BasicPropConfigurationBase<TypeString extends string, ValueType> = {
  type: TypeString
  name: string
  optional?: boolean
  default?: ValueType
}

export type StringPropConfiguration = BasicPropConfigurationBase<'string', string>
export type BooleanPropConfiguration = BasicPropConfigurationBase<'boolean', boolean>
export type NumberPropConfiguration = BasicPropConfigurationBase<'number', number>
export type BasicPropConfiguration = BooleanPropConfiguration | NumberPropConfiguration | StringPropConfiguration

export type ComponentConfiguration = {
  type: 'component',
  name: string,
  props: Record<string, BasicPropConfiguration | SlotConfiguration>,
  styles: ComponentStyleFunction
}

export type ComponentState = Record<string, unknown>

export type ComponentStyleAttributes = {
  className?: string
  style?: Record<string, unknown>
}

export type ComponentStyleFunction = (props: Record<string, unknown>) => ComponentStyleAttributes

export type Slot = {}

export type ComponentProps = {
  props: Record<string, unknown>
  styles: ComponentStyleFunction
  behaviours: Array<string>
}

export type Component = (props: ComponentProps) => JSX.Element

export type BrowserComponent = {}

export type ServerComponent = {}

export const toggleStyles = stylex.create({
  on: {
    backgroundColor: 'yellow',
    color: 'black',
  },
  off: {
    backgroundColor: 'black',
    color: 'yellow',
  },
})

export const styles = ({ toggle }: { toggle: Signal }) =>
  stylex.props(toggle.value ? styles.on : styles.off)

export const config: ComponentConfiguration = {
  type: 'component',
  name: 'Lightbulb',
  props: {
    text: {
      type: 'string',
      name: 'text',
      optional: true,
      default: 'Hello World',
    },
  },
  styles: (props) => {
    return {
      color: props.toggle ? 'yellow' : 'black',
    }
  },
}
export const Lightbulb: Component = ({ props, styles, behaviours }) => {
  const styleAttributes = computed(() =>
    styles({
      toggle: behaviours.toggle.value,
    }))
  return (
    <div {...styleAttributes.value}>{props.text.value}</div>
  )
}
