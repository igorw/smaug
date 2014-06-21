<?hh

namespace igorw\smaug;

require __DIR__.'/../src/parser.php';

var_dump(iterator_to_array(run_parser(seq(string('foo'), string('bar')), 'foobar')));

$s = delay_parser(function () use (&$s) {
    return alt(seq($s, string('a')),
               string('a'));
});

var_dump(iterator_to_array(take(1, run_parser($s, 'a'))));
var_dump(iterator_to_array(take(1, run_parser($s, 'aa'))));
var_dump(iterator_to_array(take(2, run_parser($s, 'a'))));
var_dump(iterator_to_array(take(2, run_parser($s, 'aa'))));
var_dump(iterator_to_array(take(2, run_parser($s, 'aaa'))));
