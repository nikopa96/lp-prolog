% lihtlause --> nimisonafraas, tegusonafraas.
% nimisonafraas --> nimisona, omadussonafraas, nimisona.
% nimisonafraas --> nimisona, nimisonafraas; [].
% nimisona --> [pakapiku]; [habe]; [tema]; [sobimatuse]; [jouluvanaks].

% omadussonafraas --> maarsona, omadussona.
% maarsona --> [liiga].
% omadussona --> [lyhike].
% tegusonafraas --> tegusona, nimisonafraas.
% tegusona --> [tingib]; [pohjustab].

liitlause --> lihtlause.
liitlause --> lihtlause, koma, liitlause.

lihtlause --> nimisonafraas, tegusonafraas.
nimisonafraas --> nimisona, omadussonafraas, nimisona.
nimisonafraas --> (kirjeldus; nimisona), nimisonafraas; [].
nimisona --> [pakapiku]; [habe]; [tema]; [sobimatuse]; [jouluvanaks]; [uhkus]; [sammal]; [raha]; [volad].

omadussonafraas --> maarsona, omadussona.
maarsona --> [liiga].
omadussona --> [lyhike].

kirjeldus --> kirjeldus_alam_1, kirjeldus_alam_2.
kirjeldus_alam_1 --> [veerevale].
kirjeldus_alam_2 --> [kivile].

tegusonafraas --> tegusona, nimisonafraas.
tegusonafraas --> tegusona, tapsus, nimisonafraas.
tegusona --> [tingib]; [pohjustab]; [ajab]; [tuleb]; [laheb]; [jaavad]; negatiivne_tegusona.
negatiivne_tegusona --> ei, [kasva].
tapsus --> [upakile].

ei --> [ei].
koma --> [, ].