$DistinctCountBinary -in $DistinctCountLog -from "$1T00:00:00Z" -to "$2T00:00:00Z" |grep sum| awk '{print $3}'
