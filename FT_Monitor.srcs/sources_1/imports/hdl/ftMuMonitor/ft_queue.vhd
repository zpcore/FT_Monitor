-------------------------------------------------------------------------------
-- Project    : CevTes RVU (Runtime Verification Unit)
-------------------------------------------------------------------------------
-- File       : ft_queue.vhd
-- Author     : Andreas Hagmann (ahagmann@ecs.tuwien.ac.at)
-- Copyright  : 2012, Thomas Reinbacher (treinbacher@ecs.tuwien.ac.at)
--              Vienna University of Technology, ECS Group
-------------------------------------------------------------------------------
-- Description: provides configurable queues (circular buffers)
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_pkg.all;
use work.mu_monitor_pkg.all;
use work.cevlib_unsigned_pkg.all;
use work.ft_mu_monitor_pkg.all;
use work.ram_pkg.all;

entity ft_queue is
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
end entity;

architecture arch of ft_queue is

  type register_t is record
    last_number : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
    empty       : std_logic;
  end record;

  signal this, this_nxt : register_t;

  constant registerReset : register_t := (
  (std_logic_vector(to_unsigned(0,log2c(NUMBER_OF_QUEUES)))),--(others => '0'),
    '0'
    );

  signal config_addr   : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal config_wrData : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) + log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
  signal config_rdData : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) + log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);

  signal writeP_rdAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal writeP_wrAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal writeP_wrData : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) + FT_TUPLE_T_LEN - 1 downto 0);
  signal writeP_rdData : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) + FT_TUPLE_T_LEN - 1 downto 0);
  signal writeP_write  : std_logic;

  signal readP_rdAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal readP_wrAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal readP_wrData : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
  signal readP_rdData : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
  signal readP_write  : std_logic;

  signal input_level_rdAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal input_level_wrAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal input_level_wrData : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
  signal input_level_rdData : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
  signal input_level_write  : std_logic;

  signal data_addr   : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
  signal data_wrData : std_logic_vector(FT_TUPLE_T_LEN - 1 downto 0);
  signal data_rdData : std_logic_vector(FT_TUPLE_T_LEN - 1 downto 0);
  signal data_write  : std_logic;
  
  signal sync_data_rdAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal sync_data_wrAddr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal sync_data_wrData : std_logic_vector(ft_logic_t'LENGTH - 1 downto 0);
  signal sync_data_rdData : std_logic_vector(ft_logic_t'LENGTH - 1 downto 0);
  signal sync_data_write  : std_logic;
  
  signal map_sync_data_wrAddr : std_logic_vector(log2c(ROM_LEN)-1 downto 0);
  signal map_async_wr_data : std_logic_vector(FT_TUPLE_T_LEN-1+1 downto 0);

begin

  -- instantiate components
  -- size_mask and start_addr
  config_memory_inst : sp_ram_wt
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUES),
      DATA_WIDTH => log2c(NUMBER_OF_QUEUE_ELEMENTS) + log2c(NUMBER_OF_QUEUE_ELEMENTS)
      )
    port map(
      clk    => clk,
      addr   => config_addr,
      wrData => config_wrData,
      write  => i.config_write,                                                    
      rdData => config_rdData
      );

  -- write pointer and tail element
  writeP_memory_inst : dp_ram
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUES),
      DATA_WIDTH => log2c(NUMBER_OF_QUEUE_ELEMENTS) + FT_TUPLE_T_LEN
      )
    port map(
      clk    => clk,
      rdAddr => writeP_rdAddr,
      wrAddr => writeP_wrAddr,
      wrData => writeP_wrData,
      write  => writeP_write,
      rdData => writeP_rdData
      );

  -- read pointer
  readP_memory_inst : dp_ram
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUES),
      DATA_WIDTH => log2c(NUMBER_OF_QUEUE_ELEMENTS)
      )
    port map(
      clk    => clk,
      rdAddr => readP_rdAddr,
      wrAddr => readP_wrAddr,
      write  => readP_write,
      rdData => readP_rdData,
      wrData => readP_wrData
      );

  -- input level
  input_level_inst : dp_ram
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUES),
      DATA_WIDTH => TIMESTAMP_WIDTH
      )
    port map(
      clk    => clk,
      rdAddr => input_level_rdAddr,
      wrAddr => input_level_wrAddr,
      write  => input_level_write,
      rdData => input_level_rdData,
      wrData => input_level_wrData
      );
      
  -- data for sync output
  sync_data_inst : dp_ram
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUES),
      DATA_WIDTH => ft_logic_t'LENGTH
      )
    port map(
      clk    => clk,
      rdAddr => sync_data_rdAddr,
      wrAddr => sync_data_wrAddr,
      write  => sync_data_write,
      rdData => sync_data_rdData,
      wrData => sync_data_wrData
      );

  -- data memory
  data_memory_inst : sp_ram_wt
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUE_ELEMENTS),
      DATA_WIDTH => FT_TUPLE_T_LEN
      )
    port map(
      clk    => clk,
      addr   => data_addr,
      wrData => data_wrData,
      write  => data_write,
      rdData => data_rdData
      );

  sync_data_ram : dp_ram
	generic map(
	  ADDR_WIDTH => log2c(ROM_LEN),
	  DATA_WIDTH => ft_logic_t'LENGTH
	  )
	port map(
	  clk => clk,
	  rdAddr => map_rdAddr,
	  rdData => map_sync_data,
	  write => sync_data_write,
	  wrAddr => map_wrAddr,
	  wrData => sync_data_wrData
	  );
	  
  async_data_ram : dp_ram
	generic map(
	  ADDR_WIDTH => log2c(ROM_LEN),
	  DATA_WIDTH => FT_TUPLE_T_LEN+1
	  )
	port map(
	  clk => clk,
	  rdAddr => map_rdAddr,
	  rdData => map_async_data,
	  write => sync_data_write,
	  wrAddr => map_wrAddr,
	  wrData => map_async_wr_data
	  );
 
  -- logic

  combinatorial : process (this, i, config_rdData, writeP_rdData, readP_rdData, data_rdData, input_level_rdData, sync_data_rdData)
    variable nxt           : register_t;
    variable start_address : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
    variable size_mask     : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
    variable writeP        : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
    variable writeP_inc    : std_logic_vector(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
    variable tailElement   : std_logic_vector(FT_TUPLE_T_LEN - 1 downto 0);
begin
    nxt := this;

    -- memory reads
    size_mask     := config_rdData(2*log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto log2c(NUMBER_OF_QUEUE_ELEMENTS));
    start_address := config_rdData(log2c(NUMBER_OF_QUEUE_ELEMENTS) - 1 downto 0);
    writeP        := writeP_rdData(log2c(NUMBER_OF_QUEUE_ELEMENTS) + FT_TUPLE_T_LEN - 1 downto FT_TUPLE_T_LEN);
    tailElement   := writeP_rdData(FT_TUPLE_T_LEN - 1 downto 0);

    -- defaults
    o.async.head.value <= data_rdData(FT_TUPLE_T_LEN-1);
    o.async.head.time  <= data_rdData(FT_TUPLE_T_LEN - 1 - 1 downto 0);
    o.async.tail.value <= writeP_rdData(FT_TUPLE_T_LEN-1);
    o.async.tail.time  <= writeP_rdData(FT_TUPLE_T_LEN - 1 -1 downto 0);
    o.sync <= sync_data_rdData;
    data_wrData  <= i.data.value & i.data.time;

    if writeP = readP_rdData then
      nxt.empty := '1';
    else
      nxt.empty := '0';
    end if;

    o.async.empty <= this.empty;
	map_async_wr_data <= (data_wrData & i.add); 
	
    nxt.last_number := i.number;

    -- todo: make configureable
    start_address := this.last_number & "00000";
    size_mask     := "00000011111";

    writeP_inc    := writeP + 1 and size_mask;
    writeP_wrData <= writeP_inc & i.data.value & i.data.time;

    readP_wrData <= readP_rdData + 1 and size_mask;

    writeP_write      <= '0';
    readP_write       <= '0';
    data_write        <= '0';
    input_level_write <= '0';
    sync_data_write   <= '0';

    -- address memories
    config_addr        <= i.number;
    writeP_rdAddr      <= i.number;
    writeP_wrAddr      <= this.last_number;
    readP_rdAddr       <= i.number;
    readP_wrAddr       <= this.last_number;
    data_addr          <= start_address + readP_rdData;
    input_level_rdAddr <= i.number;
    input_level_wrAddr <= this.last_number;
    input_level_wrData <= i.input_level;
    sync_data_rdAddr   <= this.last_number;
    sync_data_wrAddr   <= this.last_number;
    sync_data_wrData   <= i.sync_data;

    -- process control signals
    if i.add = '1' then
      if i.data.time > input_level_rdData or input_level_rdData = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then  -- do not add old data
        writeP_write <= '1';
        data_write   <= '1';
        if nxt.empty = '0' and tailElement(FT_TUPLE_T_LEN-1) = i.data.value then  -- queue aggregation
          data_addr     <= start_address + writeP - 1;
          writeP_wrData <= writeP & i.data.value & i.data.time;
        else
          data_addr <= start_address + writeP;
        end if;
      end if;
    end if;

    if i.flush = '1' then
      readP_wrData <= writeP;
      readP_write  <= '1';
    end if;

    if i.delete_head = '1' then
      readP_write <= '1';
    end if;

    if i.update_input_level = '1' then
      input_level_write <= '1';
    end if;
    
    if i.sync_result_write = '1' then
        sync_data_write <= '1';
    end if;

    this_nxt <= nxt;
  end process;

  reg : process (clk , res_n)
  begin
    if res_n = '0' then
      this <= registerReset;
    elsif rising_edge (clk) then
      this <= this_nxt;
    end if;
  end process;

end architecture;
