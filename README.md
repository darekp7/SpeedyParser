## Synopsis

SpeedyParser is a simple and easy to learn parser which follows the intuitive way of defining grammar (see code example below). It has quite good 
performance because the whole parser's code is compiled in SpeedyParser's constructor.

## Code Example

Imagine that you want to extract names of the tables used in SQL query. Intuitively you can describe the grammar for this problem like this:
```
    'SELECT' _columns
    [
        'FROM' tablename
        [
           'JOIN' tablename 'ON' _condition 
        ]*
        [
            'WHERE' _condition
        ]?
    ]?
```
where `_columns` and `_condition` are unimportand pieces of text.

The SpeedyParser code solving this problem is like this:
```
    var options = new SpeedyParser.ParserOptions
    {
        IsCaseSensitive = false,
        DoubleQuoteSensitivity = SpeedyParser.QuoteSensitivity.Insensitive,
        SingleQuoteSensitivity = SpeedyParser.QuoteSensitivity.SqlLike,
        Comments = new[] { new Tuple<string,string>("--", "") }
    };
    
    var p = new SpeedyParser(options, p => 
        p.If("SELECT",
            p.Span("_columns"),
            p.If("FROM",
                p.Span("tablename"),
                p.While("JOIN",
                    p.Span("tablename"),
                    p.If("ON",
                        p.Span("_condition"))),
                p.If("WHERE",
                        p.Span("_condition")))
        )
        && p.Eof);
```

## Motivation

I just wanted to have the parser like this and I couldn't find it in the net. So, I developed my own one.

## Installation

Just add SpeedyParser.cs to your project and enjoy.

## API Reference

Coming (not so) soon ;-)

## License

Public domain / WTFPL / unlicensed.
