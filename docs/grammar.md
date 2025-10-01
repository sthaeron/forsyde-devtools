# Grammar

## Supported Haskell Language Features
The compiler and visualiser supports a simplified subset of the Haskell language. The language features currently supported are the minimum required for writing SDF models using ForSyDe along with simple functions. The currently supported language features are as follows, with some provided examples also listed:
- **Types**
	- Base integer type: `Int`
    - Tuple types: `(a, b)` (at least 1 element)
    - List types: `[a, b]` (at least 1 element)
- **Expressions**
    - Identifiers and integer literals
    - Binary operations: `x + y`, `x - y`, `x * y`
    - Unary operations: `-x`
    - Tuples and lists as values: `(x, y)`, `[x, y]` (at least 1 element)
    - Function applications: ```f x```
- **Statements**
	- Function definitions: `f x = y`
	- Value bindings: `x = y`
	- Type declarations: `f :: a -> b`
## Supported ForSyDe Models
The compiler and visualiser currently only supports SDF models written using ForSyDe Shallow. The processor constructors are considered reserved identifiers similar to how keywords are implemented.
- **ForSyDe Types**
	- Signal type wrapper: `Signal Int`
- **SDF Functions**
	- The SDF related function which are supported are `delaySDF` and all actors from `actor11SDF` to `actor44SDF`.

The syntax and semantics for the subset of Haskell and ForSyDe used for this project is defined within the [grammar specification](grammar-specification.pdf) file found under the `docs` directory in this repo.
