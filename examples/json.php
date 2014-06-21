<?hh

// JSON

// @todo add namespace
// namespace igorw\gll;

require 'parser-combinator.php';

$p = new \ArrayObject();

$p['value'] = delay_parser(() ==>
    red(alt($p['string'], $p['number'], $p['object'], $p['array'], $p['true'], $p['false'], $p['null']),
        $v ==> ['value', func_get_args()])
);

$p['object'] = delay_parser(() ==>
    alt(
        red(string('{}'), () ==> ['object', []]),
        red(seq(string('{'), $p['members'], string('}')), ($l, $v, $r) ==> ['object', $v])
    )
);

$p['members'] = delay_parser(() ==>
    alt(
        red($p['pair'], $v ==> ['members', $v]),
        red(seq($p['pair'], string(','), $p['members']), ($p, $v) ==> ['members', $p, $v])
    )
);

$p['pair'] = delay_parser(() ==>
    red(seq($p['string'], string(':'), $p['value']),
        ($k, $_, $v) ==> ['pair', $k, $v])
);

$p['array'] = delay_parser(() ==>
    alt(
        red(string('[]'), () ==> ['array', []]),
        red(seq(string('['), $p['elements'], string(']')), ($l, $v, $r) ==> ['array', $v])
    )
);

$p['elements'] = delay_parser(() ==>
    alt(
        red($p['value'], $v ==> ['elements', $v]),
        red(seq($p['value'], string(','), $p['elements']), ($v, $e) ==> ['elements', $v, $e])
    )
);

$p['string'] = delay_parser(() ==>
    alt(
        red(string('""'), () ==> ['string', '']),
        red(seq(string('"'), $p['chars'], string('"')), ($l, $v, $r) ==> ['string', $v])
    )
);

$p['chars'] = red(regexp('[^\"\\/\b\f\n\r\t]+'), $v ==> ['chars', $v]);
$p['number'] = red(regexp('[0-9]+'), $v ==> ['number', $v]);

$p['true'] = red(string('true'), $v ==> ['true', $v]);
$p['false'] = red(string('false'), $v ==> ['false', $v]);
$p['null'] = red(string('null'), $v ==> ['null', $v]);

// var_dump(iterator_to_array(run_parser($p['number'], '1')));
// var_dump(iterator_to_array(run_parser($p['array'], '[]')));
// var_dump(iterator_to_array(run_parser($p['object'], '{}')));

var_dump(iterator_to_array(run_parser($p['value'], '[]')));
var_dump(iterator_to_array(run_parser($p['value'], '{}')));
var_dump(iterator_to_array(run_parser($p['value'], '"foo"')));
var_dump(iterator_to_array(run_parser($p['value'], '{"foo":"bar"}')));
var_dump(iterator_to_array(run_parser($p['value'], '{"foo":["bar","baz",42,{}]}')));
