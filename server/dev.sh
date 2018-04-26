#!/bin/bash
#一个结合rebar3与实际项目的模块
if [ $# -lt 1 ]
then
    echo "usage $0 [shell]"
    exit
fi

OP=$1
CMD=$3
DEV_CFG=dev.ini
IM_SERVER_KEY="925a2baefa242c88ae32906e4e7a1158"

ALL_SOLR_CORE="guild hero_heat"
ALL_APP="game_svr"
REL_DIR=_build/default/rel
LIB_DIR=_build/default/lib
HOST_INI_FILE=apps/game_svr/priv/host.ini
COOKIE=h5game_cookie
START_GUILDID=10000

if [ -z $2 ] || [ $2 = all ]; then
  SERVER=$ALL_APP
else
  SERVER=$2
fi

init_host_ini() {
  Cfg="%% start, end, node_id, node_ip"
  echo $Cfg > $HOST_INI_FILE
}

append_host_ini() {
  if [[ $1 != "id" ]] ; then
    HOST_IP=`/sbin/ifconfig -a|grep inet|grep -v 127.0.0.1|grep -v inet6|grep inet -m 1|awk '{print $2}'|tr -d "addr:"`
    Cfg="{$2, $3, $1, \"$HOST_IP\"}."
    echo -e $Cfg >> $HOST_INI_FILE
  fi
}

# 初始化host.ini 文件
init_host_ini
while IFS="," read f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13
do
    # 增量写入host.ini 文件
    append_host_ini $f2 $f11 $f12
    # echo $f1
    #If the line starts with ST then echo the line
    if [[ $f1 = $USER ]] ; then
        export RELX_REPLACE_OS_VARS=true
        export SERVER_ID=$f2
        export GAME_SERVER_PORT=$f3
        export DB_HOST=$f4
        export DB_PORT=$f5
        export DB_USER=$f6
        export DB_PSW=$f7
        export REDIS_HOST=$f8
        export REDIS_PORT=$f9
        export REDIS_DB=${f10//[$'\t\r\n']}
        export START_VSVRID=$f11
        export END_VSVRID=$f12
        export BATTLE_CHECK_IP=$f13
    fi
done < "$DEV_CFG"

SERVER_IP=`/sbin/ifconfig -a|grep inet|grep -v 127.0.0.1|grep -v inet6|grep inet -m 1|awk '{print $2}'|tr -d "addr:"`
CACHE_MGR_IP=$SERVER_IP
CACHE_MGR_ID=$SERVER_ID
CENTER_SERVER_IP=$SERVER_IP
CENTER_SERVER_ID=$SERVER_ID
GMT_SERVER_PORT=$(($GAME_SERVER_PORT+20))
CHAT_SERVER_PORT=$(($GAME_SERVER_PORT+10))
CHAT_SERVER_NODE=chat_svr_${SERVER_ID}@${SERVER_IP}
GAME_SERVER_NODE=game_svr_${SERVER_ID}@${SERVER_IP}

source ./config/sys.config.tmpl

prepare_cfg() {
  for SERVER_NAME in $ALL_APP
    do
      eval SVR_REG_TYPE='$'{"$SERVER_NAME"_reg_type}
      SVR_REG_TYPE=$SVR_REG_TYPE|sed -e 's/\n//g'
      eval Cfgs='$'{"$SERVER_NAME"_cfgs}
      Cfgs=${Cfgs//\$SVR_REG_TYPE/$SVR_REG_TYPE}
      Cfgs=${Cfgs//\$SERVER_NAME/$SERVER_NAME}
      # db_svr 的cli_reg_id暂时写死
      if [ $SERVER_NAME == "db_svr" ]; then
        Cfgs=${Cfgs//cli_reg_id, $SERVER_ID/cli_reg_id, 1}
      fi
      echo -e "$Cfgs" > config/_output/${SERVER_NAME}.config
      SED_VMARGS=" s/\${SERVER_ID}/${SERVER_ID}/g;"
      SED_VMARGS+=" s/\${SERVER_IP}/${SERVER_IP}/g;"
      SED_VMARGS+=" s/\${SERVER_NAME}/${SERVER_NAME}/g;"
      sed -e "${SED_VMARGS}" config/vm.args.tmpl > config/_output/${SERVER_NAME}_vm.args
    done     
}

shell() {
   rebar3 shell --name ${SERVER}_${SERVER_ID}@${SERVER_IP} --setcookie $COOKIE --apps ${SERVER} --config config/_output/${SERVER}.config
}

cmd() {
  ${REL_DIR}/$SERVER/bin/$SERVER $1
}

startsvr() {
  for i in $SERVER
  do
    echo start $i
    ${REL_DIR}/$i/bin/$i start
    sleep 1
    ${REL_DIR}/$i/bin/$i status
  done
}

stopsvr() {
  for i in $SERVER
  do
    echo stop $i
    ${REL_DIR}/$i/bin/$i stop
  done
}

release() {
  for i in $SERVER
  do
    echo "rebar3 release -n $i"
    rebar3 release -n $i
  done
}

tar_rel() {
  for i in $SERVER
  do
    echo "rebar3 as prod tar -n $i"
    rebar3 as prod tar -n $i
  done
}

build_db(){
  SQL_DIR="apps/game_svr/sql"
  SOLR_ADDRESS="http://192.168.5.5:8983/solr"
  SOLR_CONFIG_DIR="tools/solr"
  SOLR_CONFIG_SETS="/usr/local/solr-7.2.1/server/solr/configsets"

  echo "try to init/clear redis"
  redis-cli -h $REDIS_HOST -p $REDIS_PORT -n $REDIS_DB flushdb
  redis-cli -h $REDIS_HOST -p $REDIS_PORT -n $REDIS_DB set "sys:new_vsvr" $START_VSVRID
  redis-cli -h $REDIS_HOST -p $REDIS_PORT -n $REDIS_DB set "count:guild" $START_GUILDID
  echo "try to init/clear game db"
  DB_NAME="hgame_game_db_${SERVER_ID}"
  echo "use ${DB_NAME};" | cat - ${SQL_DIR}/game.sql | mysql -h${DB_HOST} -u${DB_USER} -p${DB_PSW}
  
  for i in $ALL_SOLR_CORE
  do
    CORE=${i}_${SERVER_ID}
    curl -v "${SOLR_ADDRESS}/admin/cores?action=UNLOAD&core=${CORE}&deleteIndex=true&deleteDataDir=true&deleteInstanceDir=true"
    rm -rf ${SOLR_CONFIG_SETS}/${CORE}
    cp ${SOLR_CONFIG_DIR}/${i} ${SOLR_CONFIG_SETS}/${CORE} -r
    curl -v "${SOLR_ADDRESS}/admin/cores?action=CREATE&name=${CORE}&configSet=${SOLR_CONFIG_SETS}/${CORE}"
  done
}

add_hrl(){
  find . -type d \( -path ./_build -o -path ./libs/csv_lib -o -path ./3rd \) -prune -o -name "*.hrl" | awk -F"/" '{print "-include(\""$NF"\")."}' | grep hrl > tmp
  echo "-include(\"csv.hrl\")." >> tmp
}

update_code() {
  file_name=$1

  if [[ -d $file_name ]]; then
    file_path=`find $file_name| grep .erl$`
  else
    file_path=`find  apps/${SERVER}/src/| grep -w $file_name| grep .erl$`
    if [[ $file_path == "" ]]; then
      echo "error: \"$file_name\" isn't a filename"
      exit
    fi
  fi
  node=${SERVER}_${SERVER_ID}@${SERVER_IP}
  output_shell=_build/default/lib/${SERVER}/ebin/
  output_rel=_build/default/rel/game_svr/lib/${SERVER}-0.1.0/ebin/
  for var in $file_path
  do
    compile_and_load_file $var $node $output_shell $SERVER
    compile_and_load_file $var $node $output_rel $SERVER
  done
}

compile_and_load_file() {
  file_path=$1
  node=$2

  include="-I libs/csv_lib/include/ -I libs/share/include/ -I libs/proto/include/ -I apps/${4}/include/"
  mod_name=`basename $file_path | cut -d '.' -f 1`

  erlc -o $3 -W $include $file_path

  if ! relx_nodetool $node "ping" > /dev/null; then
    echo "Node($node) is not running!"
    exit 1
  fi

  relx_nodetool $node rpcterms code soft_purge ${mod_name}.
  relx_nodetool $node rpcterms code load_file ${mod_name}.
}

relx_nodetool() {
    node=$1
    cmd=$2; shift 2

    escript ./nodetool -name $node -setcookie $COOKIE $cmd $@
}

clean() {
  rm -rf _build
  echo "_build/... cleaned!"
}

pack_tar() {
  # tar_rel
  rm _build/all_tars_in_one.tar.gz
  mkdir _build/tmp_tars
  find _build/prod/ -name "*.tar.gz" -exec cp {} _build/tmp_tars \;
  cd _build/tmp_tars && tar -zcvf ../all_tars_in_one.tar.gz ./
  rm -rf ../tmp_tars
  echo "pack all svr tars done!"
}

prepare_cfg
case $OP in
  r|rel|release )
    release
    ;;
  shell )
    shell
    ;;
  start )
    startsvr
    ;;
  stop )
    stopsvr
    ;;
  a|attach )
    cmd attach
    ;;
  rc|remote_console )
    cmd remote_console
    ;;
  f|foreground )
    cmd foreground
    ;;
  c|console )
    cmd console
    ;;
  cleandb|build_db )
    build_db
    ;;
  include )
    add_hrl
    ;;
  tar )
    tar_rel
    ;;
  u|update_code )
    update_code $CMD
    ;;
  clean )
    clean
    ;;
  p|pack )
    pack_tar
    ;;
  * )
    echo "Unknow Command."
esac
