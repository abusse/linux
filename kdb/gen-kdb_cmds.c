#include <linux/stddef.h>
#include <linux/init.h>
static __initdata char kdb_cmd0[] = "defcmd archkdb \"\" \"First line arch debugging\"\n";
static __initdata char kdb_cmd1[] = "  set BTSYMARG 1\n";
static __initdata char kdb_cmd2[] = "  set BTARGS 9\n";
static __initdata char kdb_cmd3[] = "  pid R\n";
static __initdata char kdb_cmd4[] = "  -archkdbcommon\n";
static __initdata char kdb_cmd5[] = "  r\n";
static __initdata char kdb_cmd6[] = "  -bta\n";
static __initdata char kdb_cmd7[] = "endefcmd\n";
static __initdata char kdb_cmd8[] = "defcmd archkdbcpu \"\" \"archkdb with only tasks on cpus\"\n";
static __initdata char kdb_cmd9[] = "  set BTSYMARG 1\n";
static __initdata char kdb_cmd10[] = "  set BTARGS 9\n";
static __initdata char kdb_cmd11[] = "  pid R\n";
static __initdata char kdb_cmd12[] = "  -archkdbcommon\n";
static __initdata char kdb_cmd13[] = "  -btc\n";
static __initdata char kdb_cmd14[] = "endefcmd\n";
static __initdata char kdb_cmd15[] = "defcmd archkdbshort \"\" \"archkdb with less detailed backtrace\"\n";
static __initdata char kdb_cmd16[] = "  set BTSYMARG 0\n";
static __initdata char kdb_cmd17[] = "  set BTARGS 0\n";
static __initdata char kdb_cmd18[] = "  pid R\n";
static __initdata char kdb_cmd19[] = "  -archkdbcommon\n";
static __initdata char kdb_cmd20[] = "  -bta\n";
static __initdata char kdb_cmd21[] = "endefcmd\n";
static __initdata char kdb_cmd22[] = "defcmd archkdbcommon \"\" \"Common arch debugging\"\n";
static __initdata char kdb_cmd23[] = "  set LINES 2000000\n";
static __initdata char kdb_cmd24[] = "  set BTAPROMPT 0\n";
static __initdata char kdb_cmd25[] = "  -summary\n";
static __initdata char kdb_cmd26[] = "  -id %rip-24\n";
static __initdata char kdb_cmd27[] = "  -cpu\n";
static __initdata char kdb_cmd28[] = "  -ps\n";
static __initdata char kdb_cmd29[] = "  -dmesg 600\n";
static __initdata char kdb_cmd30[] = "  -bt\n";
static __initdata char kdb_cmd31[] = "  -cpu_pda *\n";
static __initdata char kdb_cmd32[] = "endefcmd\n";
extern char *kdb_cmds[]; char __initdata *kdb_cmds[] = {
  kdb_cmd0,
  kdb_cmd1,
  kdb_cmd2,
  kdb_cmd3,
  kdb_cmd4,
  kdb_cmd5,
  kdb_cmd6,
  kdb_cmd7,
  kdb_cmd8,
  kdb_cmd9,
  kdb_cmd10,
  kdb_cmd11,
  kdb_cmd12,
  kdb_cmd13,
  kdb_cmd14,
  kdb_cmd15,
  kdb_cmd16,
  kdb_cmd17,
  kdb_cmd18,
  kdb_cmd19,
  kdb_cmd20,
  kdb_cmd21,
  kdb_cmd22,
  kdb_cmd23,
  kdb_cmd24,
  kdb_cmd25,
  kdb_cmd26,
  kdb_cmd27,
  kdb_cmd28,
  kdb_cmd29,
  kdb_cmd30,
  kdb_cmd31,
  kdb_cmd32,
  NULL
};
