<?hh

// JSON

// @todo add namespace
// namespace igorw\gll;

require 'parser-combinator.php';

// value = string | number | object | array | true | false | null
// object = 'such wow' | 'such' members 'wow'
// members = pair | pair ',' members | pair '.' members | pair '!' members | pair '?' members
// pair = string 'is' value
// array = 'so many' | 'so' elements 'many'
// elements = value | value 'and' elements | value 'also' elements
// string = '\"\"' | '\"' chars '\"'
// chars = char | char chars
// char = #'[^\"\\/\b\f\n\r\t]'
// number = int | int frac | int exp | int frac exp
// int = digit | digit1-9 digits | '-' digit | '-' digit1-9 digits
// digit1-9 = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
// digit = digit1-9 | '0'
// frac = '.' digits
// exp = very digits
// digits = digit | digit digits
// very = 'very' | 'very+' | 'very-' | 'VERY' | 'VERY+' | 'VERY-'
// true = 'yes'
// false = 'no'
// null = 'empty'

$value = delay_parser(function () use (&$string, &$number, &$object, &$array, &$true, &$false, &$null) {
    return red(alt($string, $number, $object, $array, $true, $false, $null),
               $v ==> ['value', func_get_args()]);
});

$object = delay_parser(function () use (&$members) {
    return alt(
        red(string('{}'), () ==> ['object', []]),
        red(seq(string('{'), $members, string('}')), ($l, $v, $r) ==> ['object', $v])
    );
});

$members = delay_parser(function () use (&$members, &$pair) {
    return alt(
        red($pair, $v ==> ['members', $v]),
        red(seq($pair, string(','), $members), ($p, $v) ==> ['members', $p, $v])
    );
});

$pair = delay_parser(function () use (&$string, &$value) {
    return red(seq($string, string(':'), $value),
               ($k, $_, $v) ==> ['pair', $k, $v]);
});

$array = delay_parser(function () use (&$elements) {
    return alt(
        red(string('[]'), () ==> ['array', []]),
        red(seq(string('['), $elements, string(']')), ($l, $v, $r) ==> ['array', $v])
    );
});

$elements = delay_parser(function () use (&$value, &$elements) {
    return alt(
        red($value, $v ==> ['elements', $v]),
        red(seq($value, string(','), $elements), ($v, $e) ==> ['elements', $v, $e])
    );
});

$string = delay_parser(function () use (&$chars) {
    return alt(
        red(string('""'), () ==> ['string', '']),
        red(seq(string('"'), $chars, string('"')), ($l, $v, $r) ==> ['string', $v])
    );
});

$chars = red(regexp('[^\"\\/\b\f\n\r\t]+'), $v ==> ['chars', $v]);
$number = red(regexp('[0-9]+'), $v ==> ['number', $v]);

$true = red(string('true'), $v ==> ['true', $v]);
$false = red(string('false'), $v ==> ['false', $v]);
$null = red(string('null'), $v ==> ['null', $v]);

// var_dump(iterator_to_array(run_parser($number, '1')));
// var_dump(iterator_to_array(run_parser($array, '[]')));
// var_dump(iterator_to_array(run_parser($object, '{}')));

var_dump(iterator_to_array(run_parser($value, '[]')));
var_dump(iterator_to_array(run_parser($value, '{}')));
var_dump(iterator_to_array(run_parser($value, '"foo"')));
var_dump(iterator_to_array(run_parser($value, '{"foo":"bar"}')));
var_dump(iterator_to_array(run_parser($value, '{"foo":["bar","baz",42,{}]}')));
