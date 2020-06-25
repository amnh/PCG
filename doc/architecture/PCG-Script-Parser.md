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

The PCG scripting language is designed using a [Free Monad](https://www.tweag.io/blog/2018-02-05-free-monads) to construct a syntax for the langauge. The syntax is composed of primitive value types and recursive combinators. The langauge was designed with the following principles in mind: *sensible defaults, permutable argument lists, and exceptional error messages*.

The PCG scripting language has 6 rpimitive values. They are primitive in the sense that they cannot be composed of smaller components of the PCG syntax. They are the "base cases" of the recursively defined language.