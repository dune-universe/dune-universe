use MaxMind::DB::Writer::Tree;

my %types = (
    iso_code => 'utf8_string',
    latitude => 'float',
    longitude => 'float',
    country => 'map',
    subdivisions  => [ 'array', 'map' ],
    location  => 'map',
    names => [ 'map', 'map' ],
    en => 'utf8_string'
);

my $tree = MaxMind::DB::Writer::Tree->new(
    ip_version            => 6,
    record_size           => 24,
    database_type         => 'ocaml-mmdb-test-data',
    languages             => ['en'],
    description           => { en => 'Test data for ocaml-mmdb' },
    map_key_type_callback => sub { $types{ $_[0] } },
);

$tree->insert_network(
    '172.56.31.240/32',
    {
        country => { iso_code => 'Country', names => { en => 'Atlantis' } },
        subdivisions => [ { iso_code => 'Region' } ],
        location => { latitude => 1.2, longitude => 3.4 },
    },
);

open my $fh, '>:raw', 'sample.mmdb';
$tree->write_tree($fh);
