-------------------------------------------------------------------------------
-- Project    : CevTes RVU (Runtime Verification Unit)
-------------------------------------------------------------------------------
-- File       : mu_monitor_tb.vhd
-- Author     : Patrick Moosbrugger (p.moosbrugger@gmail.com)
-- Copyright  : 2011, Thomas Reinbacher (treinbacher@ecs.tuwien.ac.at)
--              Vienna University of Technology, ECS Group
-------------------------------------------------------------------------------
-- Description: testbench for the mu_monitor
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.mu_monitor_pkg.all;
use work.ft_mu_monitor_pkg.all;
use work.testbench_util_pkg.all;
use std.textio.all;
--use work.cevrvu_top_pkg.all;
--library modelsim_lib;
--use modelsim_lib.util.all;
use work.math_pkg.all;

entity tb is
  generic (
    imemFilePath  : string := "D:\XilinxProject\FT_Monitor\FT_Monitor.srcs\sources_1\imports\FT_Monitor\imem.imem";
    inputFilePath : string := "D:\XilinxProject\FT_Monitor\FT_Monitor.srcs\sources_1\imports\FT_Monitor\until_0.trc";
    intFilePath   : string := "D:\XilinxProject\FT_Monitor\FT_Monitor.srcs\sources_1\imports\FT_Monitor\imem.imem.int"
    --imemFilePath  : string := "D:\XilinxProject\FT_Monitor\FT_Monitor.srcs\sources_1\imports\FT_Monitor\TACAS_examples.imem";
    --inputFilePath : string := "D:\XilinxProject\FT_Monitor\FT_Monitor.srcs\sources_1\imports\FT_Monitor\TACAS_examples.trc";
    --intFilePath   : string := "D:\XilinxProject\FT_Monitor\FT_Monitor.srcs\sources_1\imports\FT_Monitor\TACAS_examples.imem.int"
    );
end entity;

architecture sim of tb is
  constant CLK_FREQ                   : integer                                         := 100000000;
  constant SUT_FREQ                   : integer                                         := 100000;
--  constant SUT_FREQ                   : integer                                         := 2000000;
  constant ATOMICS_WIDTH              : integer                                         := 2;
  signal   s_clk, s_reset_n           : std_logic                                       := '0';
  signal   s_sutclk                   : std_logic                                       := '0';
  signal   s_rtc_timestamp            : std_logic_vector(TIMESTAMP_WIDTH-1 downto 0)    := (others => '0');
  signal   s_atomics                  : std_logic_vector(ATOMICS_WIDTH-1 downto 0)      := (others => '0');
  signal   s_programming_stop         : boolean                                         := false;
  signal   stop                       : boolean                                         := false;
  signal   s_programming_imem         : std_logic;
  signal   s_programming_interval_mem : std_logic;
  signal   s_en                       : std_logic                                       := '0';
  signal   s_trigger                  : std_logic                                       := '0';
  signal   s_programming_memory_addr  : std_logic_vector(MEMBUS_ADDR_WIDTH-1 downto 0)  := (others => '0');
  signal   s_programming_memory_data  : std_logic_vector (2*TIMESTAMP_WIDTH-1 downto 0) := (others => '0');
  signal   s_violated                 : std_logic                                       := '0';
  signal   s_violated_valid           : std_logic                                       := '0';
  file mumonProgram_file              : text open read_mode is imemFilePath;
  file mumonInterval_file             : text open read_mode is intFilePath;
  file testSample_file                : text open read_mode is inputFilePath;
  file testSampleResult_file                : text open read_mode is inputFilePath;
  signal   s_rtc_en                   : std_logic                                       := '0';
  
  -- spy signals
  signal new_result_spy	: std_logic;
  signal result_time_spy : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
  signal result_value_spy : std_logic;

--  component simple_mem is
--  generic(addr_length_bits : integer := 8);
--  port(CLK            : in  std_logic;
--       w_addr         : in  std_logic_vector(addr_length_bits-1 downto 0); -- write address    
--       w_data         : in  std_logic); -- write data
-- end simple_mem;


  function reverse(input : std_logic_vector) return std_logic_vector is
    variable output : std_logic_vector(input'length - 1 downto 0);
  begin
    for i in 0 to (input'length - 1) loop
      output(input'length-1 - i) := input(i);
    end loop;
    return output;
  end function;

begin
  dut : ft_mu_monitor
    port map
    (
      clk                     => s_clk,
      reset_n                 => s_reset_n,
      atomics                 => s_atomics,
      timestamp               => s_rtc_timestamp,
      en                      => s_en,
      trigger				=> s_trigger,
      program_addr            => s_programming_memory_addr,
      program_data            => s_programming_memory_data,
      program_imem            => s_programming_imem,
      program_interval_memory => s_programming_interval_mem,
      violated                => s_violated,
      violated_valid          => s_violated_valid,
      pt_data_memory_addr     => open,
    pt_data_memory_data     => '0',
    sync_out                => open,
    finish                  => open,
    data_memory_async_data  => open,
    data_memory_async_empty => open,
    data_memory_sync_data   => open,
    data_memory_addr        => (others => '0'),
    
    --Pei: signal for simulation to replace spy function 
    this_new_result         => new_result_spy,
    this_sync_out_data_time => result_time_spy
    --this_sync_out_data_value => result_value_spy
      );
      
--   spy : process
--  begin
--    init_signal_spy("/tb/dut/ift_mu_monitor_fsm/this.new_result", "/tb/new_result_spy");
--    init_signal_spy("/tb/dut/ift_mu_monitor_fsm/this.sync_out.data.time", "/tb/result_time_spy");
--    init_signal_spy("/tb/dut/ift_mu_monitor_fsm/this.sync_out.data.value", "/tb/result_value_spy");
--    wait;
--  end process;

  process
  begin
    s_clk <= '0';
    wait for 1 sec / (2 * CLK_FREQ);
    s_clk <= '1';
    if stop = true then
      wait;
    end if;
    wait for 1 sec / (2 * CLK_FREQ);
  end process;

trigger_signal : process
begin
  wait until s_sutclk = '1';
  s_trigger <= '1';
  wait_cycle(s_clk, 15);
  s_trigger <= '0';
  wait until s_sutclk = '0';
end process;

  p_generate_sut_clk : process
  begin
    s_sutclk <= '0';
    wait for 1 sec / (2 * SUT_FREQ);
    s_sutclk <= '1';
    if stop = true then
      wait;
    end if;
    wait for 1 sec / (2 * SUT_FREQ);
  end process p_generate_sut_clk;

  p_rtc : process (s_sutclk, s_rtc_en)
  begin
    if s_sutclk'event and s_sutclk = '1' then
      if s_rtc_en = '1' then
        s_rtc_timestamp <= increment_slv(s_rtc_timestamp);
        if(s_rtc_timestamp = std_logic_vector(to_unsigned(2147483647, s_rtc_timestamp'length))) then
          log_err("s_rtc_timestamp overflow!");
        end if;
      end if;
    end if;
  end process p_rtc;

  p_write_mumonitor_prom : process
    variable v_file_line_rd    : line;
    variable v_str_line        : string (COMMAND_WIDTH downto 1);
    variable v_str_line_ts     : string (32 downto 1);
    variable v_mumon_prom_addr : std_logic_vector(s_programming_memory_addr'length-1 downto 0) := (others => '0');
  begin

    s_programming_imem         <= '0';
    s_programming_interval_mem <= '0';
    s_reset_n                  <= '0';
    wait_cycle(s_clk, 1);
    s_reset_n                  <= '1';
    wait_cycle(s_clk, 20);

    while not endfile(mumonProgram_file) loop
      readline(mumonProgram_file, v_file_line_rd);
      read(v_file_line_rd, v_str_line);

      s_programming_imem        <= '1';
      s_programming_memory_addr <= v_mumon_prom_addr;
      s_programming_memory_data <= "000000000000000000000000" & str_to_std_logic_vector(v_str_line);
      wait_cycle(s_clk, 1);
      -- increment prom address;
      v_mumon_prom_addr         := increment_slv(v_mumon_prom_addr);
    end loop;

    s_programming_imem <= '0';
    v_mumon_prom_addr  := (others => '0');

    while not endfile(mumonInterval_file) loop
      readline(mumonInterval_file, v_file_line_rd);
      read(v_file_line_rd, v_str_line_ts);

      s_programming_interval_mem <= '1';
      s_programming_memory_addr  <= v_mumon_prom_addr;
--      s_programming_memory_data  <= "000000000000000000000000" & str_to_std_logic_vector(v_str_line_ts)(32-1 downto 16) & "000000000000000000000000" & str_to_std_logic_vector(v_str_line_ts)(16-1 downto 0);
      s_programming_memory_data  <= "0000000000000000" & str_to_std_logic_vector(v_str_line_ts)(32-1 downto 16) & "0000000000000000" & str_to_std_logic_vector(v_str_line_ts)(16-1 downto 0);
      wait_cycle(s_clk, 1);
      -- increment prom address;
      v_mumon_prom_addr          := increment_slv(v_mumon_prom_addr);
    end loop;

    s_programming_interval_mem <= '0';

    wait_cycle(s_clk, 10);

    s_programming_stop <= true;
    wait;
    -- finished programming

  end process;

  p_apply_test_input : process
    variable v_file_line_rd        : line;
    variable v_str_line            : string(ATOMICS_WIDTH downto 1);
    variable v_str_atomics         : string(ATOMICS_WIDTH downto 1);
    variable v_str_timestamp_delta : string(TIMESTAMP_WIDTH downto 1);
    variable v_atomics             : std_logic_vector(ATOMICS_WIDTH-1 downto 0);
    variable v_timestamp_delta     : std_logic_vector(TIMESTAMP_WIDTH-1 downto 0);
    variable v_last_output_time    : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0) := (others => '0');
  begin
    wait until s_programming_stop = true;
    wait until (s_sutclk'event and s_sutclk = '1');

    -- set initial atomics values
    s_atomics <= (others => '0');
    wait_cycle(s_sutclk, 2);

    -- activate mu_monitor
    s_en     <= '1';
    s_rtc_en <= '1';

    --log_to_file_ts("Start applying test input, enabling RTC", s_rtc_timestamp, log_file);
    while not endfile(testSample_file) loop
      readline(testSample_file, v_file_line_rd);
      read(v_file_line_rd, v_str_line);
      -- disassemble the read line
      v_str_atomics := v_str_line(ATOMICS_WIDTH downto 1);
      v_atomics     := str_to_std_logic_vector(v_str_atomics);

      -- apply new atomic propositions
      s_atomics <= reverse(v_atomics);
      -- wait for given delta
      wait_cycle(s_sutclk, 1);

      -- check result
      if new_result_spy = '1' or v_last_output_time = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then
        while v_last_output_time <= result_time_spy  or v_last_output_time = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) loop
	        readline(testSampleResult_file, v_file_line_rd);
    	  	read(v_file_line_rd, v_str_line);
      		-- disassemble the read line
      		v_str_atomics := v_str_line(ATOMICS_WIDTH downto 1);
      		v_atomics     := str_to_std_logic_vector(v_str_atomics);
      
--            if result_value_spy = not v_atomics(0) then
--              report "testcase FAILED" severity warning;
--            end if;
            
            v_last_output_time := v_last_output_time + 1;
        end loop;
      end if;

    end loop;

    --if s_violated_valid = '0' then
    --  report "testcase FAILED" severity warning;
    --end if;

    s_en <= '0';
    stop <= true;

    wait;
  end process;
end architecture;
