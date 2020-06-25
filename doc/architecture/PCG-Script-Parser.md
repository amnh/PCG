# PCG Scripting Language Parser
---

### Related packages:
 * `language`

### Related modules:

 - **`PCG.Command.Build`**
 - **`PCG.Command.Echo`**
 - **`PCG.Command.Load`**
 - **`PCG.Command.Read`**
 - **`PCG.Command.Report`**
 - **`PCG.Command.Save`**
 - **`PCG.Syntax`**
 - **`PCG.Syntax.Combinators`**
 - **`PCG.Syntax.Parser`**
 - **`PCG.Syntax.Primitive`**

The PCG scripting language is designed using a [Free Monad](https://www.tweag.io/blog/2018-02-05-free-monads) to construct a syntax for the language. The syntax is composed of primitive value types and recursive combinators. The language was designed with the following principles in mind: *sensible defaults, permutable argument lists, and exceptional error messages*.

The PCG scripting language has 6 primitive values. They are primitive in the sense that they cannot be composed of smaller components of the PCG syntax. They are the "base cases" of the recursively defined language.
 - **`bool`:** A boolean value
 - **`int`:** An integral valued number
 - **`real`:** A "real" valued number 
 - **`text`:** A textual value
 - **`time`:** A temporal value
 - **`value`:** A context-sensitive, literal value

Additionally, there are 8 combinators used to build up a recursively defined definition of a command specifiable by the PCG syntax.

  - **`argList`:** A list of arguments as part of a command
  - **`command`:** Specifies a command in the PCG scripting language defined by the command name and the argument structure
  - **`choiceFrom`:** Matches exactly one of the provided arguments to be used in the command
  - **`argId`:** Require a prefix on an argument value to disambiguate it from other argument values
  - **`argIds`:** Require a prefix on an argument value to disambiguate it from other argument values. Accepts multiple aliases for the prefix used to disambiguate the argument.
  - **`manyOf`:** Produce zero or more of the provided argument for the command
  - **`someOf`:** Produce one or more of the provided argument for the command
  - **`withDefault`:** Provide a default value for the argument if it is missing from the user input

Commands for the PCG scripting language are constructed by combining combinators and primitives together in a structure that reflects the data the program needs to extract from the script file. PCG constructs data structures which represent what is required to begin executing a command. The `language` library's primitive and combinators are used to build up a command structure which reflects the data type. The data type is then `fmap`ed or `<*>`ed inside the Free Monad structure of the `language` library. Once the data structure has been embedded with the Free Monad, a parser for the data structure can be automatically derived. All PCG commands are constructed in this way, providing a consistent parser with a consistent syntax and consistent error message reporting for all commands.

All arguments of a combinator are commutative, meaning that the arguments can be recursively permuted into any permissible order. This design decision makes arguments order independent, improving user experience when specifying commands by not having to remember the order. However, this requires all arguments to be unambiguous, which can often be determined by the differing of the types of arguments, but when multiple arguments to a combinator have the same type it requires to using the `argId` function to have disambiguate identifiers for the similarly typed arguments.

The "plumbing" code beneath the primitive and combinator code is quite complex, performing a large number of varying checks. This makes the execution of the parser a bit slow. However, these checks are required to provide informative parse error messages to the user. There is still work to be done improving the error messages in many cases but they are currently in a passbale state.

Commands for the PCG scripting language are defined in the `PCG.Command.*` modules. Here you can find examples of commands being defines as data-types, the `language` library primitives and combinators reflecting the data-type, and the data type being embedded within the Free Monad structure.
