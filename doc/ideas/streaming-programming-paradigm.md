# StreamWeave

StreamWeave is a toolkit centered around the use of Streams for building general
purpose computer programs.

It uses the Web Streams API to provide a unified interface for handling data
streams, enabling developers to build robust and scalable applications with
ease. StreamWeave's core principles revolve around the following key concepts:

1. **Stream Composition**: StreamWeave embraces the power of streams by
   providing a set of composable building blocks that can be combined to create
   complex data processing pipelines.
2. **Stream Reactivity**: Streams are inherently reactive, meaning that data can
   be processed in real-time as it arrives.
3. **Stream Backpressure**: StreamWeave ensures that data flows are managed
   efficiently, preventing overload or slowdown due to backpressure.
4. **Stream Error Handling**: Streams can handle errors gracefully, ensuring
   uninterrupted data flow and recovery from failures.  5. **Stream
   Parallelism**: StreamWeave supports parallel processing of streams, allowing
   for efficient utilization of multiple resources.
5. **Stream Integration**: StreamWeave integrates seamlessly with other Web
   technologies, such as Web Workers and WebAssembly, enabling the creation of
   powerful and efficient applications.
6. **Stream Interoperability**: StreamWeave is designed to work with a wide
   range of data sources and destinations, making it suitable for a variety of
   use cases.

## Basics

### Source

A Source is an interface that connect a data source to a stream. It defines the
methods and properties that are required to read data from the source and emit
it to the stream. A Source returns a ReadableStream.

```graphviz
digraph Source {
  compound=true;

  Data;

  subgraph cluster_Source {
    label = "Source";
    ReadableStream;
  }

  Data -> ReadableStream [ltail=Data, lhead=cluster_Source, label="data"];
}
```

### Operator

An Operator is a component that transforms or processes data in a stream. It
defines the methods and properties that are required to perform the
transformation or processing. An Operator takes a Source or another Operator as
an input and it returns a TransformStream.

```graphviz
digraph Operator {
  compound=true;

  subgraph cluster_Source {
    label = "Source";
    ReadableStream;
  }

  subgraph cluster_Operator {
    label = "Operator";
    TransformStream;
  }

  ReadableStream -> TransformStream [ltail=cluster_Source,
  lhead=cluster_Operator, label="data"];
}
```

### Sink

A Sink is a component that consumes data from a stream. It defines the methods
and properties that are required to read data from the stream and write it to a
destination. A Sink returns a WritableStream.

```graphviz
digraph Sink {
  compound=true;

  subgraph cluster_Sink {
    label = "Sink";
    WritableStream;
  }

  WritableStream -> Output [ltail=cluster_Sink, lhead=Output, label="data"];
}
```

### Pipeline

Naturally, these three components can be combined to create a pipeline.

```graphviz
digraph Pipeline {
  compound=true;

  subgraph cluster_Source {
    label = "Source";
    "ReadableStream";
  }

  subgraph cluster_Operator {
    label = "Operator";
    "TransformStream";
  }

  subgraph cluster_Sink {
    label = "Sink";
    "WritableStream";
  }

  ReadableStream -> TransformStream [ltail=cluster_Source,
  lhead=cluster_Operator, label="data"];
  TransformStream -> WritableStream [ltail=cluster_Operator, lhead=cluster_Sink,
  label="data"];
}
```

## Transforming Data

### Map

Mapping is a simple transformation that applies a function to each element of a
stream. It is a fundamental building block for transforming data in a stream.

```graphviz
digraph Mapping {
  compound=true;

  "[1,2,3]"

  subgraph cluster_Source {
    label = "Source";
    ReadableStream;
  }

  subgraph cluster_Map {
    label = "Map (x) => x * 2";
    TransformStream;
  }

  subgraph cluster_Sink {
    label = "Sink";
    WritableStream;
  }

  "[1,2,3]" -> ReadableStream [ltail=ReadableStream, lhead=cluster_Source,
  label="1,2,3"];
  ReadableStream -> TransformStream [ltail=cluster_Source, lhead=cluster_Map,
  label="1,2,3"];
  TransformStream -> WritableStream [ltail=cluster_Map, lhead=cluster_Sink,
  label="2,4,6"];
}
```

### Reduce

Reducing is a transformation that accumulates the elements of a stream into a
single result. It is useful for calculating aggregated metrics or summarizing
information from a stream.

```graphviz
digraph Reducing {
  compound=true;
  "[1,2,3]"

  subgraph cluster_Source {
    label = "Source";
    ReadableStream;
  }

  subgraph cluster_Reduce {
    label = "Reduce (x, y) => x + y";
    TransformStream;
  }

  subgraph cluster_Sink {
    label = "Sink";
    WritableStream;
  }

  "[1,2,3]" -> ReadableStream [ltail=ReadableStream, lhead=cluster_Source,
  label="1,2,3"];
  ReadableStream -> TransformStream [ltail=cluster_Source, lhead=cluster_Reduce,
  label="1,2,3"];
  TransformStream -> WritableStream [ltail=cluster_Reduce, lhead=cluster_Sink,
  label="6"];
}
```

### Filter

Filtering is a transformation that allows only elements that meet a certain
condition to pass through the stream. It is useful for refining data flows,
ensuring that only relevant data reaches downstream operations.

```graphviz
digraph Filtering {
  compound=true;

  "[1,2,3]"

  subgraph cluster_Source {
    label = "Source";
    ReadableStream;
  }

  subgraph cluster_Filter {
    label = "Filter (x) => x > 1";
    TransformStream;
  }

  subgraph cluster_Sink {
    label = "Sink";
    WritableStream;
  }

  "[1,2,3]" -> ReadableStream [ltail=ReadableStream, lhead=cluster_Source,
  label="1,2,3"];
  ReadableStream -> TransformStream [ltail=cluster_Source, lhead=cluster_Filter,
  label="1,2,3"];
  TransformStream -> WritableStream [ltail=cluster_Filter, lhead=cluster_Sink,
  label="2,3"];
}
```

### Split

Splitting is a transformation that divides a stream into multiple outputs. It is
useful for broadcasting data or applying different processing logic to the same
input source. You can apply different kinds of logic to divide or duplicate the
incoming data to the output streams. These can be implemented in different
components like Broadcast or RoundRobin.

```graphviz
digraph Splitting {
  compound=true;

  "[1,2,3]"

  subgraph cluster_Source {
    label = "Source";
    ReadableStream;
  }

  subgraph cluster_Split {
    label = "Broadcast 3";
    TransformStream1;
    TransformStream2;
    TransformStream3;
  }

  subgraph cluster_Sink1 {
    label = "Sink 1";
    WritableStream1;
  }

  subgraph cluster_Sink2 {
    label = "Sink 2";
    WritableStream2;
  }

  subgraph cluster_Sink3 {
    label = "Sink 3";
    WritableStream3;
  }

  "[1,2,3]" -> ReadableStream [ltail=ReadableStream, lhead=cluster_Source,
  label="1,2,3"];
  ReadableStream -> TransformStream2 [ltail=cluster_Source, lhead=cluster_Split,
  label="1,2,3"];
  TransformStream1 -> WritableStream1 [ltail=cluster_Split, lhead=cluster_Sink1,
  label="1,2,3"];
  TransformStream2 -> WritableStream2 [ltail=cluster_Split, lhead=cluster_Sink2,
  label="1,2,3"];
  TransformStream3 -> WritableStream3 [ltail=cluster_Split, lhead=cluster_Sink3,
  label="1,2,3"];
}
```

### Merge

Merging is a transformation that combines multiple streams into a single output
stream. It is useful for combining data from different sources or synchronizing
data streams. Like Split, you can apply different kinds of logic to merge or
combine the incoming data to the output stream. These can be implemented in
different components like Join or Concat.

```graphviz
digraph Merging {
  compound=true;

  subgraph cluster_Source1 {
    label = "Source 1";
    ReadableStream1;
  }

  subgraph cluster_Source2 {
    label = "Source 2";
    ReadableStream2;
  }

  subgraph cluster_Source3 {
    label = "Source 3";
    ReadableStream3;
  }

  subgraph cluster_Merge {
    label = "Join";
    TransformStream;
  }

  subgraph cluster_Sink {
    label = "Sink";
    WritableStream;
  }

  ReadableStream1 -> TransformStream [ltail=cluster_Source1,
  lhead=cluster_Merge, label="1,2,3"];
  ReadableStream2 -> TransformStream [ltail=cluster_Source2,
  lhead=cluster_Merge, label="4,5,6"];
  ReadableStream3 -> TransformStream [ltail=cluster_Source3,
  lhead=cluster_Merge, label="7,8,9"];
  TransformStream -> WritableStream [ltail=cluster_Merge, lhead=cluster_Sink,
  label="1,4,7,2,5,8,3,6,9"];
}
```

## Parallelism

Streams in JavaScript are inherently parallel due to the asynchronous nature of
the JavaScript runtime. This characteristic allows data processing tasks to run
concurrently without blocking the main execution thread. By leveraging
asynchronous I/O, streams can efficiently handle large volumes of data across
multiple operations in parallel.

### Inherent Parallelization

The streaming paradigm allows developers to naturally take advantage of
JavaScript's event-driven model, enabling parallel processing of data streams as
they arrive. Each component in a streaming pipeline (i.e., Source, Operator,
Sink) processes data in its own execution context, allowing multiple operations
to occur simultaneously. This results in faster data processing and improved
performance, especially in I/O-bound applications.

### Encapsulating Parallelization Techniques

While streams provide a level of parallelism by their own design, additional
parallelization techniques can be implemented to enhance performance further:

- **Web Workers**: By offloading heavy computations to Web Workers, developers
  can leverage multi-core CPU capabilities. Web Workers operate in separate
  threads, allowing tasks to run in parallel to the main thread. This helps in
  reducing the load on the main thread, providing smoother performance for
  critical tasks like UI rendering.

- **Node.js Cluster**: In Node.js environments, the Cluster module can be used
  to create child processes that share the same server port. These cluster
  processes can handle separate incoming requests, making better use of
  multi-core systems. This approach can be particularly beneficial for
  CPU-intensive tasks, allowing them to be spread across multiple processors.

By integrating these parallelization strategies with the inherent parallel
nature of streams, StreamWeave can provide robust support for efficient and
scalable data processing applications.

By implementing these techniques as Composition instead of Parameterization or
Runtime, we can achieve a more flexible and modular approach to parallelization,
allowing developers to tailor the parallelization strategy to their specific
needs. This also increases maintainability and reusability of the codebase of
StreamWeave itself, as it can be implemented as a library of components.

## Integration with Other Technologies

StreamWeave can be integrated with other technologies to enhance its
functionality and performance. Some examples include:

- **WebSockets**: StreamWeave can be used to create WebSocket-based data
  processing pipelines, allowing real-time data exchange between clients and
  servers. This integration can be particularly useful for applications that
  require real-time updates, such as chat applications or live streaming.
- **WebRTC**: StreamWeave can be used to create WebRTC-based data processing
  pipelines, enabling real-time communication between peers. This integration
  can be useful for applications that require real-time collaboration, such as
  video conferencing or live streaming.
- **WebAssembly**: StreamWeave can be used to create WebAssembly-based data
  processing pipelines, allowing for efficient and scalable data processing in
  the browser. This integration can be particularly useful for applications that
  require high-performance computing, such as machine learning or scientific
  simulations.

## Components

Inspiration was taken from [NoFlo](https://noflojs.org/) for the components.

### Source Components

#### Kick

Generates a single item and pushes it to the stream.

#### Timeout

Generates an item after a specified delay and pushes it to the stream.

#### Interval

Generates an item at a specified interval and pushes it to the stream.

#### EnvironmentVariable

Reads an environment variable and pushes its value to the stream.

#### GlobalVariable

Reads a global variable and pushes its value to the stream.

#### String

Creates a string.

#### Number

Creates a number.

#### Boolean

Creates a boolean.

#### Object

Creates an object.

#### Array

Creates an array.

#### Date

Creates a Date object.

#### Timestamp

Creates a timestamp.

#### Error

Creates an Error object.

#### Function

Creates a Function.

### Operator Components

#### Repeat

Outputs all items in the input stream to the output stream.

#### Delay

Delays each item in the stream by a specified amount of time.

#### Throttle

Throttles the output stream to a specified rate.

#### ExtendedObject

Extends an object with additional properties.

#### FilteredObject

Filters an object based on a predicate function.

#### FlattenedObject

Flattens an object of objects into a single object.

#### ValueFromObject

Gets a value from an object.

#### KeysFromObject

Gets an array of keys from an object.

#### ValuesFromObject

Gets an array of values from an object.

#### SizeOfObject

Gets the size of an object.

#### ConcatenatedArray

Concatenate each Array in the stream with an array.

#### ValueFromArray

Gets a value from an array.

#### FlattenedArray

Flattens an array of arrays into a single array.

#### SizeOfArray

Gets the size of an array.

#### ErrorFromString

Creates an Error object from a string.

#### StringFromError

Creates a string from an Error object.

#### DateFromTimestamp

Creates a Date object from a timestamp.

#### DateFromString

Creates a Date object from a string.

#### TimestampFromDate

Creates a timestamp from a Date object.

#### StringFromDate

Creates a string from a Date object.

### Sink Components

#### Drop

Drops each item in the stream.

#### Callback

Calls a Callback for each data item in the stream.

#### ObjectFunction

Calls a Function on an Object for each data item in the stream.

#### Log

Logs each item in the stream using the console.
