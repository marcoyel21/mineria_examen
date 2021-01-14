#!bin/bash
#Selecciona un destino
#cd Desktop
#mkdir parcial
#cd parcial/
curl https://raw.githubusercontent.com/marcoyel21/intro-to-data-science-2020/master/primer_parcial/data/imports-85.names?token=ANVOYCRMS462IKSBVI5TWV27STPTS | sed '61,89!d' | perl -lne '/(?<=\.)(.+?)(?=:)/ and print $1' |sed 's/^.//' | sed 's/.*/"&"/'|sed '1s/^/colnames<- c(/;$!s/$/,/;$s/$/)/'  > metadata.R
