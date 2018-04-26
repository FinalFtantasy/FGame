#!/bin/sh

client_dir=../Client
csv_lib=libs/csv_lib

sync_proto()
{
  ##拉取客户端协议，并且建立软连接
  p4 sync ${client_dir}/csproto/...
  echo "sync client proto, ok."
  ln -f ../Client/csproto/* libs/proto/msg/
  echo "hard link client proto, ok."
}

sync_csv()
{
  # 拉取最新csv
  p4 sync ${client_dir}/csv/...

  # 拷贝cs到指定位置
  cp ${client_dir}/csv/ $csv_lib -rf
}

csv2record()
{
  # 转换csv文件到erlang record格式
  csv2record=3rd/csv2record
  cd $csv2record
  rebar3 escriptize
  cd -
  csv2record_exe=${csv2record}/_build/default/bin/csv2record
  csv_opt="ebin_dir=${csv_lib}/ebin;hrl_dir=${csv_lib}/include;src_dir=${csv_lib}/src;"
  $csv2record_exe ${csv_lib}/csv $csv_opt && cat ${csv_lib}/include/csv_*.hrl > ${csv_lib}/include/csv.hrl
}

help()
{
    echo ""
    echo "Default to sync proto, sync csv, transform csv to record."
    echo ""
    echo "Optional parameters:"
    echo "  proto       only sync proto file."
    echo "  csv         only sync csv file."
    echo "  record      only transform csv to record."
    echo "  help        print help info."
    echo ""
}

case $1 in
  proto) sync_proto;;
  csv) sync_csv;;
  record) csv2record;;
  help) help;;
  *) sync_proto;sync_csv;csv2record
esac
