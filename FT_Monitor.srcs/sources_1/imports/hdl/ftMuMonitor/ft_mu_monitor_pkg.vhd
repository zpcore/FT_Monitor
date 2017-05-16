-------------------------------------------------------------------------------
-- Project    : CevTes RVU (Runtime Verification Unit)
-------------------------------------------------------------------------------
-- File       : mu_monitor_pkg.vhd
-- Author     : Andreas Hagmann (ahagmann@ecs.tuwien.ac.at)
-- Copyright  : 2012, Thomas Reinbacher (treinbacher@ecs.tuwien.ac.at)
--              Vienna University of Technology, ECS Group
-------------------------------------------------------------------------------
-- Description:  Package for mu_monitor-constants and components.                    
------------------------------------------------------------------------------- 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_pkg.all;
use work.cevLib_unsigned_pkg.all;
use work.mu_monitor_pkg.all;
use work.log_input_pkg.all;

package ft_mu_monitor_pkg is
  constant NUMBER_OF_QUEUES         : integer := 64;
  constant NUMBER_OF_QUEUE_ELEMENTS : integer := 2048;

  type ft_tuple_t is record
    value : std_logic;  --what's this??
    time  : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
  end record;
  constant FT_TUPLE_T_NULL : ft_tuple_t := ('0', (others => '0'));
  constant FT_TUPLE_T_LEN  : integer    := 1 + TIMESTAMP_WIDTH;

  -- synchronous output logic
  subtype  ft_logic_t is std_logic_vector(2-1 downto 0);
  constant FT_TRUE  : ft_logic_t := "01";
  constant FT_FALSE : ft_logic_t := "00";
  constant FT_MAYBE : ft_logic_t := "10";

  type ft_queue_in_t is record
    number             : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
    data_in_op1        : ft_tuple_t;
    data_in_op2        : ft_tuple_t;
  end record;

  type ft_queue_async_out_t is record
    head  : ft_tuple_t;
    tail  : ft_tuple_t;
    empty : std_logic;
  end record;
  constant FT_QUEUE_ASYNC_OUT_T_NULL : ft_queue_async_out_t := (FT_TUPLE_T_NULL, FT_TUPLE_T_NULL, '0');

  type ft_queue_out_t is record
    async : ft_queue_async_out_t;
    sync  : ft_logic_t;
  end record;
  constant FT_QUEUE_OUT_T_NULL : ft_queue_out_t := (FT_QUEUE_ASYNC_OUT_T_NULL, FT_FALSE);

  component ft_queue is
    port (
      clk   : in  std_logic;
      res_n : in  std_logic;
      i     : in  ft_queue_in_t;
      o     : out ft_queue_out_t;
	  map_rdAddr : in std_logic_vector(log2c(ROM_LEN)-1 downto 0);
	  map_wrAddr : in std_logic_vector(log2c(ROM_LEN)-1 downto 0);
	  map_sync_data : out std_logic_vector(ft_logic_t'LENGTH-1 downto 0);
	  map_async_data : out std_logic_vector(FT_TUPLE_T_LEN-1+1 downto 0)
      );
  end component;

  component ft_mu_monitor_fsm is
    port (clk                     : in  std_logic;  -- clock signal
          res_n                   : in  std_logic;  -- reset signal (active low)
          violated                : out std_logic;  -- violated signal (result is not correct)
          violatedValid           : out std_logic;
          trigger                 : in  std_logic;
          atomics                 : in  std_logic_vector(ATOMICS_WIDTH-1 downto 0);  -- invariant-checker-violated-bits from fifo
          timestamp               : in  std_logic_vector(TIMESTAMP_WIDTH-1 downto 0);  -- invariant-checker-violated-bits from fifo
          imemAddr                : out std_logic_vector(log2c(ROM_LEN) -1 downto 0);
          imemData                : in  std_logic_vector(COMMAND_WIDTH-1 downto 0);
          interval_memory_data    : in  std_logic_vector(2*TIMESTAMP_WIDTH-1 downto 0);
          interval_memory_addr    : out std_logic_vector(log2c(INTERVAL_MEMORY_LEN) -1 downto 0);
          pt_data_memory_addr     : out std_logic_vector(log2c(DATA_MEMORY_LEN) -1 downto 0);
          pt_data_memory_data     : in  std_logic;
          sync_out                : out ft_logic_t;
          async_out               : out ft_tuple_t;
          finish                  : out std_logic;
          data_memory_async_data  : out std_logic_vector(FT_TUPLE_T_LEN-1+1 downto 0);
          data_memory_async_empty : out std_logic;
          data_memory_sync_data   : out ft_logic_t;
          data_memory_addr        : in  std_logic_vector(log2c(ROM_LEN) - 1 downto 0);
          
          this_new_result         : out  std_logic;
          this_sync_out_data_time  : out std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0)
          --this_sync_out_data_value : out std_logic
          
          );
  end component;

  component ft_mu_monitor is
    port (clk                     : in  std_logic;  -- clock signal
          reset_n                 : in  std_logic;  -- reset signal (low active)
          en                      : in  std_logic;  -- en signal
          trigger                 : in  std_logic;
          atomics                 : in  std_logic_vector(ATOMICS_WIDTH-1 downto 0);  -- invariant checker result
          timestamp               : in  std_logic_vector(TIMESTAMP_WIDTH-1 downto 0);
          program_addr            : in  std_logic_vector(MEMBUS_ADDR_WIDTH-1 downto 0);  -- rom address for programming
          program_data            : in  std_logic_vector(MEMBUS_DATA_WIDTH-1 downto 0);  -- programming data for rom
          program_imem            : in  std_logic;
          program_interval_memory : in  std_logic;
          violated                : out std_logic;  -- violated signal
          violated_valid          : out std_logic;
          pt_data_memory_addr     : out std_logic_vector(log2c(DATA_MEMORY_LEN) - 1 downto 0);
          pt_data_memory_data     : in  std_logic;
          sync_out                : out ft_logic_t;
          finish                  : out std_logic;
          data_memory_async_data  : out std_logic_vector(FT_TUPLE_T_LEN-1+1 downto 0);
          data_memory_async_empty : out std_logic;
          data_memory_sync_data   : out ft_logic_t;
          data_memory_addr        : in  std_logic_vector(log2c(ROM_LEN) - 1 downto 0);
          
          --Pei: signal for simulation to replace spy function
          
          this_new_result         : out  std_logic;
          this_sync_out_data_time  : out std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0)
          --this_sync_out_data_value : out std_logic
          
          );
  end component;
  
end package;

package body ft_mu_monitor_pkg is


end package body;
