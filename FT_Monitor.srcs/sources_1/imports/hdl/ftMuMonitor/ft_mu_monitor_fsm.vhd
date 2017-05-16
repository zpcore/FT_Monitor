-------------------------------------------------------------------------------
-- Project    : CevTes RVU (Runtime Verification Unit)
-------------------------------------------------------------------------------
-- File       : mu_monitor_fsm.vhd
-- Author     : Andreas Hagmann (ahagmann@ecs.tuwien.ac.at)
-- Copyright  : 2011, Thomas Reinbacher (treinbacher@ecs.tuwien.ac.at)
--              Vienna University of Technology, ECS Group
-------------------------------------------------------------------------------
-- Description: A state machine for mu_monitor calculation.
-- PAMO:   op1Type unterscheidet typen des inputs wie ich es verstehe --> 
--         der bug dass es nur mit atomics geht koennte hier gesucht werden
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.cevLib_unsigned_pkg.all;
use work.math_pkg.all;
use work.mu_monitor_pkg.all;
use work.ft_mu_monitor_pkg.all;
use work.log_input_pkg.all;
use work.ram_pkg.all;


--port(
--    clk                     : in  std_logic;  -- clock signal
--    res_n                   : in  std_logic;  -- reset signal (active low)
--    trigger                 : in  std_logic;
--    atomics                 : in  std_logic_vector(ATOMICS_WIDTH - 1 downto 0);  -- invariant-checker-violated-bits from fifo
--    timestamp               : in  std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);  -- invariant-checker-violated-bits from fifo
--    imemAddr                : out std_logic_vector(log2c(ROM_LEN) - 1 downto 0);
--    imemData                : in  std_logic_vector(COMMAND_WIDTH - 1 downto 0);
--    interval_memory_data    : in  std_logic_vector(2*TIMESTAMP_WIDTH - 1 downto 0);
--    interval_memory_addr    : out std_logic_vector(log2c(INTERVAL_MEMORY_LEN)-1 downto 0);
--    async_out               : out ft_tuple_t;
--    finish                  : out std_logic;
--    data_memory_async_data  : out std_logic_vector(FT_TUPLE_T_LEN-1+1 downto 0);
--    data_memory_async_empty : out std_logic;
--    data_memory_addr        : in  std_logic_vector(log2c(ROM_LEN) - 1 downto 0);
    
----Pei: signal for simulation to replace spy function
--    this_new_result         : out  std_logic;
--    this_sync_out_data_time  : out std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0)
--    );
entity ft_mu_monitor_fsm is
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
    this_sync_out_data_time : out std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0)
    );
end entity;

architecture arch of ft_mu_monitor_fsm is
  -- data types
  type state_t is (RESET, IDLE, FETCH, LOAD_OP1_WAIT, LOAD_OP1, LOAD_OP2, CALC, CALC_BOX_DOT, CALC_UNTIL, WRITE_BACK, UPDATE_Q1, UPDATE_Q2, WAITS);

  type registerType is record
    state                     : state_t;
    programCounter            : std_logic_vector(log2c(ROM_LEN) - 1 downto 0);
    command                   : instruction_t;
    atomics                   : std_logic_vector(ATOMICS_WIDTH-1 downto 0);
    time                      : std_logic_vector(TIMESTAMP_WIDTH-1 downto 0);
    interval                  : interval_t;
    interval_length           : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    op1                       : ft_queue_async_out_t;
    op2                       : ft_queue_async_out_t;
    result                    : ft_tuple_t;
    new_result                : std_logic;
    async_out                 : ft_tuple_t;
  end record;

  -- variables / constants

  signal registerReset : registerType := (
    RESET,
    (std_logic_vector(to_unsigned(0,log2c(ROM_LEN)))),--(others => '0'),
    INSTRUCTION_T_NULL,
    (others => '0'),
    (others => '1'),
    INTERVAL_T_NULL,
    (others => '0'),
    FT_QUEUE_ASYNC_OUT_T_NULL,
    FT_QUEUE_ASYNC_OUT_T_NULL,
    FT_TUPLE_T_NULL,
    '0',
    FT_TUPLE_T_NULL
    );

  signal this, nxt : registerType;


  type   subformula_ram_t is array (0 to 127) of ft_tuple_t;
  signal subformula_ram : subformula_ram_t;
  signal op1Type : std_logic_vector(2-1 downto 0);
  signal op2Type : std_logic_vector(2-1 downto 0);
begin
  

  violated <= '0';
  violatedValid <= '0';
  pt_data_memory_addr <= (others=>'0');
  sync_out <= FT_MAYBE;
  data_memory_sync_data <= FT_MAYBE;
--pei: added the statement 
  this_new_result <= this.new_result;
  this_sync_out_data_time <= this.async_out.time;
  --this_sync_out_data_value <= this.sync_out.data.value;

  -- instantiate components    

  i_queue : ft_queue
    port map (
      clk => clk,
      res_n => res_n,
      i => queue_i,
      o => queue_o
      );
  -- logic
  combinatorial : process(this, atomics, timestamp, imemData, interval_memory_data, trigger, data_memory_addr)


  begin
    nxt <= this;

    -- default values for signals and variables
    imemAddr                  <= this.programCounter;
    finish                    <= '0';
    async_out                 <= this.async_out;

    op1Type <= this.command.op1.is_memory_addr & this.command.op1.is_immediate;
    op2Type <= this.command.op2.is_memory_addr & this.command.op2.is_immediate;

    interval_memory_addr <= this.command.intervalAddr(interval_memory_addr'length - 1 downto 0);

	  --i_map_wrAddr <= this.programCounter - 1;

    -- fsm
    case this.state is
      when RESET =>
        nxt.state          <= IDLE;
        
      when IDLE =>
        nxt.new_result <= '0';
        nxt.programCounter <= (others => '0');
        imemAddr           <= (others => '0');--instruction memory address
        if trigger = '1' then           -- start new run
          nxt.state               <= FETCH;
        end if;

      when FETCH =>
        nxt.new_result <= '0';
        finish <= '0';
        nxt.atomics <= atomics;
        nxt.command <= slv_to_instruction(imemData);

        nxt.time <= timestamp;

        nxt.state          <= LOAD_OP1_WAIT;
        nxt.programCounter <= this.programCounter + 1;
        
      when LOAD_OP1_WAIT =>
        nxt.state        <= LOAD_OP1;

      when LOAD_OP1 =>
        case op1Type is

          when "10" => --read from subformula ram
            nxt.op1.head.value <= this.op1.tail.value;
            nxt.op1.tail.value <= subformula_ram(to_integer(unsigned(this.command.op1.value))).value;--need test
            nxt.op1.head.time  <= timestamp;--need test
            nxt.op1.tail.time  <= timestamp;

          when "00" => -- read atomic
            nxt.op1.head.value <= this.op1.tail.value;
            nxt.op1.tail.value <= this.atomics(conv_to_integer(this.command.op1.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            nxt.op1.head.time  <= timestamp;
            nxt.op1.tail.time  <= timestamp;
            
          when others =>
            null;
        end case;


        if this.time = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then
          nxt.op1.head.value <= '0';
          nxt.op1.head.value <= '0';
        end if;

-- Pei: Why we need this????-----------------------------------------------
        -- switch to invert input signal
        case this.command.command is
          when OP_FT_DIAMONDDIAMOND |
            OP_FT_DIAMONDDOT =>
            nxt.op1.head.value <= not nxt.op1.head.value;
            nxt.op1.tail.value <= not nxt.op1.tail.value;
          when others =>
            null;
        end case;

        -- load timestamps
        nxt.interval.min <= interval_memory_data(2*TIMESTAMP_WIDTH-1 downto TIMESTAMP_WIDTH);
        nxt.interval.max <= interval_memory_data(TIMESTAMP_WIDTH-1 downto 0);
----------------------------------------------------------------------------

        case this.command.command is
          when OP_FT_UNTIL | OP_FT_AND =>
            nxt.state <= LOAD_OP2;
          when others =>
            nxt.state <= CALC;
        end case;

      when LOAD_OP2 =>
        case op2Type is
          when "10" => --read from subformula ram
            nxt.op2.head.value <= this.op2.tail.value;
            nxt.op2.tail.value <= subformula_ram(to_integer(unsigned(this.command.op2.value))).value;--need test
            nxt.op2.head.time  <= timestamp;--need test
            nxt.op2.tail.time  <= timestamp;
          when "00" =>                  -- read atomic
            nxt.op2.head.value <= this.op2.tail.value;
            nxt.op2.tail.value <= this.atomics(conv_to_integer(this.command.op1.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            nxt.op2.head.time  <= timestamp;
            nxt.op2.tail.time  <= timestamp;
            
          when others =>
            null;
        end case;

        if this.time = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then
          nxt.op2.head.value <= '0';
          nxt.op2.head.value <= '0';
        end if;

        nxt.state        <= CALC;

      when CALC =>
        nxt.state      <= WRITE_BACK;--this state sequence is not for all the operation

        case this.command.command is
          when OP_LOAD =>
            --if this.op1.empty = '0' then
            --  nxt.new_result <= '1';
            --  nxt.result     <= this.op1.head;
            --end if;

            
          when OP_END_SEQUENCE =>
 
            nxt.async_out  <= this.op1.head;

            
          when OP_FT_BOXBOX |
            OP_FT_DIAMONDDIAMOND |
            OP_FT_BOXDOT |
            OP_FT_DIAMONDDOT =>



            case this.command.command is
              when OP_FT_BOXBOX =>
                
              when OP_FT_DIAMONDDIAMOND =>
              
                
              when OP_FT_BOXDOT | OP_FT_DIAMONDDOT =>
                
              when others =>
                null;
            end case;
            
          when OP_FT_UNTIL =>
            
            
          when OP_FT_NOT =>
            nxt.result.time  <= this.op1.tail.time;
            nxt.result.value <= not this.op1.tail.value;
            
          when OP_FT_AND =>
            nxt.state <= this.state; --halt the state to wait for AND operation
            if(AND_finish='1')then--add to sensitive signal
              nxt.state <= WRITE_BACK;
            end if;
          when OP_FT_IMPLICIT =>

          when others =>
            null;
        end case;
        
      
      when WRITE_BACK =>
        nxt.state <= FETCH;
        nxt.new_result <= '1';
        
        case this.command.command is
          when OP_END_SEQUENCE=>
            nxt.state <= IDLE;
            finish <= '1';
          when others=>
            nxt.state <= FETCH;
        end case;
      when others =>
        null;
    end case;

    -- precalculations
    nxt.interval_length <= this.interval.max - this.interval.min;

  end process;

  AND_operation : process(clk, res_n)
  begin
    if(res_n = '0')then

    else 
      if(clk = '1' and clk' event)then

      end if;
    end if;
  end process;


  update_subformula_ram : process(clk, res_n)
  begin
    if(res_n = '0')then
      for i in 0 to 127 loop
        subformula_ram(i).value <= '0';
        subformula_ram(i).time <= (others => '0');
      end loop;
    else 
      if(clk = '1' and clk' event)then
        if(this.state=WRITE_BACK)then
          subformula_ram(to_integer(unsigned(this.programCounter))-1) <= this.result;
          case this.command.command is
            when OP_FT_DIAMONDDIAMOND |
              OP_FT_DIAMONDDOT =>
              --subformula_ram(to_integer(unsigned(this.programCounter))-1).async_result.value <= not this.result.value;
            when others =>
              null;
          end case;
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
