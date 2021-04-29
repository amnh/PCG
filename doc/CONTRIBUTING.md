# How to contribute

I'm really glad you're reading this, because we need volunteer developers to help this project come to fruition.

If you haven't already, contact the project author [Ward Wheeler][wheeler] to determine the best tasks for you to contribute to.
We want you working on things you're excited about.

Here are some important resources:

  * [Documentation](https://github.com/amnh/PCG/tree/master/doc) tells you how to get started with PCG, and
  * [GitHub Issues](https://github.com/amnh/PCG/issues) is our project management space.

## Testing

We have a handful of existing specific unit, property-based, and integration tests.
Adding one or more of these forms of tests with your contribution would be greating appreciated.

## Submitting changes

Please send a [GitHub Pull Request to PCG](https://github.com/amnh/PCG/pull/new/master) with a clear list of what you've done (read more about [pull requests](http://help.github.com/pull-requests/)).
When you send a pull request, we will love you forever if you include [Tasty][tasty] test suites and examples within the Haddock documentation.
We can always use more test coverage.
Please follow our coding conventions (below) and make sure all of your commits are atomic (one feature per commit).

Always write a clear log message for your commits.
One-line messages are fine for small changes, but bigger changes should look like this:

    $ git commit -m "A brief summary of the commit
    > 
    > A paragraph describing what changed and its impact."

## Coding conventions

Start reading our code and you'll get the hang of it.
We optimize for readability:

  * We indent using four spaces
  * We seperate top level definitions with *two* blank lines
  * We use a variety of tools to maintain code hygeine, there are invokations for these in the `makefile`
  * Run `make lint` prior to opening a pull request to run all code hygiene tools

Thanks,

[Ward Wheeler][wheeler]

*[Professor Richard Gilder Graduate School][rggs], [American Museum of Natural History][amnh]*

[amnh]:    https://www.amnh.org/research
[rggs]:    https://www.amnh.org/research/richard-gilder-graduate-school
[tasty]:   https://hackage.haskell.org/package/tasty
[wheeler]: https://www.amnh.org/research/staff-directory/ward-wheeler
