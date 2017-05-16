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
    clk                : in  std_logic;
    res_n              : in  std_logic;
    i                  : in  ft_queue_in_t;
    write              : in  std_logic;
    o                  : out ft_queue_out_t;
    finish             : out std_logic;
    reset_queue_number : std_logic --connect to finish signal of upper layer
    );
end entity;

architecture arch of ft_queue is
  type state_t is (RESET, IDLE, FETCH, CALC, COMPLETE);

  type register_t is record
    state : state_t;
    queue_number : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
    isOp2Content : std_logic;
  end record;

  signal this, nxt : register_t;

  signal registerReset : register_t := (
    RESET,
    '0',
    '0'
    );

  signal ctrl_addr : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
  signal ctrl_wrData : std_logic_vector(2*log2c(QUEUE_SIZE) downto 0);
  signal wr_ptr : std_logic_vector(log2c(QUEUE_SIZE)-1 downto 0);
  signal rd_ptr : std_logic_vector(log2c(QUEUE_SIZE)-1 downto 0);
begin

  --ctrl_wrData <= fq_to_slv(i);
  ctrl_addr <= this.queue_number;
  -- instantiate components
  -- size_mask and start_addr

  -- control RAM
  ctrl_ram_inst : sp_ram_wt
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUES),
      DATA_WIDTH => 2*log2c(QUEUE_SIZE) + 1 --ptr_wr, ptr_rd, 1 bit(indicates queue for op1:0 or op2:1)
      )
    port map(
      clk    => clk,
      addr   => ctrl_addr,
      wrData => ctrl_wrData,
      write  => write,--ctrl_write,                                                
      rdData => ctrl_rdData
      );

  -- data memory
  data_memory_inst : sp_ram_wt
    generic map(
      ADDR_WIDTH => log2c(NUMBER_OF_QUEUES) + log2c(QUEUE_SIZE),
      DATA_WIDTH => TIMESTAMP_WIDTH + 1
      )
    port map(
      clk    => clk,
      addr   => data_addr,
      wrData => data_wrData,
      write  => data_write,
      rdData => data_rdData
      );

  -- logic
  combinatorial : process (this, reset_queue_number)

  begin
  
    nxt <= this;
    if(reset_queue_number = '1') then
      this.state <= RESET;
    else
      case this.state is
        when RESET =>
          this.queue_number <= (others => '0');
          if(write='1')then
            nxt.state <= IDLE;
          end if;
        when IDLE =>
          this.wr_ptr;
        when FETCH_PTR =>


        when DEQUE =>

        when WRITE_PTR_BACK =>

        when COMPLETE => 
          
          queue_number <= std_logic_vector(unsigned(queue_number)+1);----automatic increase the quque number
        when others =>
          null;
      end case;
    end if;
  end process;

  and_operation : process(clk , res_n)
  begin
    if(res_n = '0')then

    else 
      if(clk = '1' and clk' event)then
        if(this.state => FETCH_PTR)then
          rd_ptr <= ctrl_rdData(2*log2c(QUEUE_SIZE) downto log2c(QUEUE_SIZE)+1);
          wr_ptr <= ctrl_rdData(log2c(QUEUE_SIZE) downto 1);
        end if;
      end if;
    end if;
  end process;






  reg : process (clk , res_n)
  begin
    if res_n = '0' then
      this <= registerReset;
    elsif rising_edge (clk) then
      this <= nxt;
    end if;
  end process;

end architecture;
