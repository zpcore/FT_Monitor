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

entity ft_mu_monitor_fsm is
  port(
    clk                     : in  std_logic;  -- clock signal
    res_n                   : in  std_logic;  -- reset signal (active low)
    violated                : out std_logic;  -- violated signal (result is not correct)
    violatedValid           : out std_logic;
    trigger                 : in  std_logic;
    atomics                 : in  std_logic_vector(ATOMICS_WIDTH - 1 downto 0);  -- invariant-checker-violated-bits from fifo
    timestamp               : in  std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);  -- invariant-checker-violated-bits from fifo
    imemAddr                : out std_logic_vector(log2c(ROM_LEN) - 1 downto 0);
    imemData                : in  std_logic_vector(COMMAND_WIDTH - 1 downto 0);
    interval_memory_data    : in  std_logic_vector(2*TIMESTAMP_WIDTH - 1 downto 0);
    interval_memory_addr    : out std_logic_vector(log2c(INTERVAL_MEMORY_LEN)-1 downto 0);
    pt_data_memory_addr     : out std_logic_vector(log2c(DATA_MEMORY_LEN) - 1 downto 0);
    pt_data_memory_data     : in  std_logic;
    sync_out                : out ft_logic_t;
    async_out               : out ft_tuple_t;
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
end entity;

architecture arch of ft_mu_monitor_fsm is
  -- data types
  type state_t is (RESET, IDLE, FETCH, LOAD_OP1_WAIT, LOAD_OP1, LOAD_OP2, CALC, CALC_BOX_DOT, CALC_UNTIL, WRITE_BACK, UPDATE_Q1, UPDATE_Q2, WAITS);
  type update_queue_t is (NO, DEQUEUE, FLUSH);
  type box_dot_state_t is (NONE, BD1, BD2);
  type until_state_t is (NONE, U1, U2, U3);

  type registerType is record
    state                     : state_t;
    next_state                : state_t;
    programCounter            : std_logic_vector(log2c(ROM_LEN) - 1 downto 0);
    command                   : instruction_t;
    violated                  : std_logic;
    violatedValid             : std_logic;
    atomics                   : std_logic_vector(ATOMICS_WIDTH-1 downto 0);
    time                      : std_logic_vector(TIMESTAMP_WIDTH-1 downto 0);
    interval                  : interval_t;
    interval_length           : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    startupCounter            : std_logic_vector(COMMAND_WIDTH-1 downto 0);
    queue_number              : std_logic_vector(log2c(NUMBER_OF_QUEUES) - 1 downto 0);
    op1                       : ft_queue_async_out_t;
    op2                       : ft_queue_async_out_t;
    op1_sync                  : ft_logic_t;
    op2_sync                  : ft_logic_t;
    result                    : ft_tuple_t;
    new_result                : std_logic;
    result_sync               : ft_logic_t;
    result_sync_out           : ft_logic_t;
    update_q1                 : update_queue_t;
    update_q2                 : update_queue_t;
    sync_out                  : ft_logic_t;
    async_out                 : ft_tuple_t;
    last_level                : std_logic;
    m_rising                  : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    m_tau                     : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    last_level_phi            : std_logic;
    m_rising_phi              : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    m_falling_phi             : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    m_pre                     : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    m_processed               : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    until_op1_ready           : std_logic;
    until_state               : until_state_t;
    until_postpone            : std_logic;
    until_processed           : std_logic;
    box_dot_state             : box_dot_state_t;
    head_time_interval_max    : std_logic_vector(TIMESTAMP_WIDTH - 1 downto 0);
    time_greater_interval_max : std_logic;
    forward_data_memory       : std_logic;
  end record;

  -- variables / constants

  constant registerReset : registerType := (
    RESET,
    IDLE,
    (std_logic_vector(to_unsigned(0,log2c(ROM_LEN)))),--(others => '0'),
    INSTRUCTION_T_NULL,
    '0',
    '1',
    (others => '0'),
    (others => '1'),
    INTERVAL_T_NULL,
    (others => '0'),
    (others => '0'),
    (std_logic_vector(to_unsigned(0,log2c(NUMBER_OF_QUEUES)))),--(others => '0'),
    FT_QUEUE_ASYNC_OUT_T_NULL,
    FT_QUEUE_ASYNC_OUT_T_NULL,
    FT_FALSE, --(others => '0'), -- this one
    FT_FALSE,--(others => '0'), -- this one
    FT_TUPLE_T_NULL,
    '0',
    FT_FALSE,--(others => '0'), -- this one
    FT_FALSE,--(others => '0'), -- this one
    NO,
    NO,
    FT_FALSE,--(others => '0'), -- this one
    FT_TUPLE_T_NULL,
    '0',
    (others => '0'),
    (others => '0'),
    '0',
    (others => '0'),
    (others => '0'),
    (others => '0'),
    (others => '0'),
    '0',
    NONE,
    '0',
    '0',
    NONE,
    (others => '0'),
    '0',
    '0'
    );

  signal this, this_nxt : registerType;

  -- control signals
  signal queue_i : ft_queue_in_t;
  signal queue_o : ft_queue_out_t;

  -- box memory signals
  signal box_addr   : std_logic_vector(log2c(BOX_MEMORY_LEN) - 1 downto 0);
  signal box_rdData : std_logic_vector(2*TIMESTAMP_WIDTH + 1 - 1 downto 0);
  signal box_wrData : std_logic_vector(2*TIMESTAMP_WIDTH + 1 - 1 downto 0);
  signal box_write  : std_logic;

  -- until memory signals
  signal until_addr   : std_logic_vector(log2c(DOT_BUFFER_LEN) - 1 downto 0);
  signal until_rdData : std_logic_vector(4*TIMESTAMP_WIDTH + 2 - 1 downto 0);
  signal until_wrData : std_logic_vector(4*TIMESTAMP_WIDTH + 2 - 1 downto 0);
  signal until_write  : std_logic;
  
  signal i_map_wrAddr : std_logic_vector(log2c(ROM_LEN)-1 downto 0);

  -- for simulation only
  type subformula_result is record
    async_result : ft_tuple_t;
    async_maybe  : std_logic;
    sync_result  : ft_logic_t;
  end record;
  type   subformula_ram_t is array (0 to 1023) of subformula_result;
  signal subformula_ram : subformula_ram_t;
  
begin

--pei: added the statement 
  this_new_result <= this.new_result;
  this_sync_out_data_time <= this.async_out.time;
  --this_sync_out_data_value <= this.sync_out.data.value;

  -- instantiate components
  i_queue : ft_queue
    port map (
      clk   => clk,
      res_n => res_n,
      i     => queue_i,
      o     => queue_o,
	  map_rdAddr => data_memory_addr,
	  map_wrAddr => i_map_wrAddr,
	  map_sync_data => data_memory_sync_data,
	  map_async_data => data_memory_async_data
	  );             

  i_box_memory : sp_ram_wt
    generic map (
      ADDR_WIDTH => log2c(BOX_MEMORY_LEN),  -- address width
      DATA_WIDTH => 2*TIMESTAMP_WIDTH + 1)      
    port map (
      clk    => clk,                        -- clock signal
      addr   => box_addr,                   -- read address
      wrData => box_wrData,                 -- write data
      write  => box_write,
      rdData => box_rdData); 

  i_until_memory : sp_ram_wt
    generic map (
      ADDR_WIDTH => log2c(DOT_BUFFER_LEN),  -- address width
      DATA_WIDTH => 4*TIMESTAMP_WIDTH + 1 + 1)      
    port map (
      clk    => clk,                        -- clock signal
      addr   => until_addr,                 -- read address
      wrData => until_wrData,               -- write data
      write  => until_write,
      rdData => until_rdData);        

  -- logic
  cominatorial : process(this, atomics, timestamp, queue_o, imemData, interval_memory_data, box_rdData, until_rdData, pt_data_memory_data, trigger, data_memory_addr, box_addr, until_addr)
    variable nxt     : registerType;
    variable op1Type : std_logic_vector(2-1 downto 0);
    variable op2Type : std_logic_vector(2-1 downto 0);

    variable a_does_not_hold : std_logic := '0';
    variable b_does_not_hold : std_logic := '0';
    variable v_op_sync : std_logic_vector(3 downto 0) := (others=>'0');
  begin
    nxt := this;

    -- default values for signals and variables
    violated                  <= this.violated;
    violatedValid             <= this.violatedValid;
    imemAddr                  <= this.programCounter;
    queue_i.sync_result_write <= '0';
    sync_out                  <= this.result_sync_out;
    finish                    <= '0';
    async_out                 <= this.async_out;

    op1Type := this.command.op1.is_memory_addr & this.command.op1.is_immediate;
    op2Type := this.command.op2.is_memory_addr & this.command.op2.is_immediate;

    interval_memory_addr <= this.command.intervalAddr(interval_memory_addr'length - 1 downto 0);

    box_addr       <= this.command.memoryAddr(box_addr'length - 1 downto 0);
    nxt.last_level := box_rdData(0);
    nxt.m_rising   := box_rdData(TIMESTAMP_WIDTH + 1 - 1 downto 1);
    nxt.m_tau      := box_rdData(2*TIMESTAMP_WIDTH + 1 - 1 downto TIMESTAMP_WIDTH + 1);

    until_addr <= this.command.memoryAddr(until_addr'length - 1 downto 0);

    box_write   <= '0';
    until_write <= '0';

    a_does_not_hold := '0';
    b_does_not_hold := '0';
    v_op_sync := (others => '0');

    pt_data_memory_addr <= this.command.op1.value(pt_data_memory_addr'length - 1 downto 0);

    -- queue management
    queue_i.add                <= '0';
    queue_i.data               <= this.result;
    queue_i.delete_head        <= '0';
    queue_i.flush              <= '0';
    queue_i.update_input_level <= '0';
    queue_i.input_level        <= this.result.time;
    queue_i.sync_data          <= this.result_sync;

    -- data_memory_async_data  <= queue_o.async.head;
     data_memory_async_empty <= queue_o.async.empty;
	  i_map_wrAddr <= this.programCounter - 1;
    -- data_memory_sync_data   <= queue_o.sync;

    -- precalculations
    nxt.head_time_interval_max := this.op1.head.time - this.interval.max;
    if this.time > this.interval.max then
      nxt.time_greater_interval_max := '1';
    else
      nxt.time_greater_interval_max := '0';
    end if;

    -- fsm
    case this.state is
      when RESET =>
        nxt.startupCounter := imemData;
        nxt.state          := IDLE;
        
      when IDLE =>
        nxt.programCounter := (others => '0');
        imemAddr           <= (others => '0');

        if trigger = '1' then           -- start new run
          nxt.state               := FETCH;
          nxt.forward_data_memory := '0';
        end if;
        
      --when WAITS =>
        --nxt.state := this.next_state;
        
      when FETCH =>
        nxt.atomics := atomics;
        nxt.command := slv_to_instruction(imemData);

        nxt.time := timestamp;

        nxt.state          := LOAD_OP1_WAIT;
        nxt.queue_number   := nxt.command.op1.value(nxt.queue_number'length - 1 downto 0);
        nxt.programCounter := this.programCounter + 1;
        
      when LOAD_OP1_WAIT =>
        nxt.queue_number := this.command.op2.value(nxt.queue_number'length - 1 downto 0);
        --nxt.next_state   := LOAD_OP1;
        --nxt.state        := WAITS;
        nxt.state        := LOAD_OP1;

      when LOAD_OP1 =>
        case op1Type is
          when "10" =>                  -- read from queue
            nxt.op1      := queue_o.async;
            nxt.op1_sync := queue_o.sync;
            
          when "11" =>                  -- read from mtl mumonitor
            -- read from ptmtl mumonitor
            nxt.op1.head.value := pt_data_memory_data;
            nxt.op1.tail.value := pt_data_memory_data;
            nxt.op1.head.time  := timestamp;
            nxt.op1.tail.time  := timestamp;
            nxt.op1.empty      := '0';
            nxt.op1_sync       := "0" & pt_data_memory_data;
            
          when "00" =>                  -- read atomic
            nxt.op1.head.value := this.atomics(conv_to_integer(this.command.op1.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            nxt.op1.tail.value := this.atomics(conv_to_integer(this.command.op1.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            nxt.op1.head.time  := timestamp;
            nxt.op1.tail.time  := timestamp;
            nxt.op1.empty      := '0';
            nxt.op1_sync       := "0" & this.atomics(conv_to_integer(this.command.op1.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            
          when "01" =>                  -- immediate
            nxt.op1.head.value := this.command.op1.value(0);
            nxt.op1.tail.value := this.command.op1.value(0);
            nxt.op1.head.time  := timestamp;
            nxt.op1.tail.time  := timestamp;
            nxt.op1.empty      := '0';
            nxt.op1_sync       := "0" & this.command.op1.value(0);
            
          when others =>
            null;
        end case;

        if this.time = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then
          nxt.op1.head.value := '0';
          nxt.op1.head.value := '0';
        end if;

-- Pei: Why we need this????-----------------------------------------------
        -- switch to invert input signal
        case this.command.command is
          when OP_FT_DIAMONDDIAMOND |
            OP_FT_DIAMONDDOT =>
            nxt.op1.head.value := not nxt.op1.head.value;
            nxt.op1.tail.value := not nxt.op1.tail.value;
          when others =>
            null;
        end case;

        -- load timestamps
        nxt.interval.min := interval_memory_data(2*TIMESTAMP_WIDTH-1 downto TIMESTAMP_WIDTH);
        nxt.interval.max := interval_memory_data(TIMESTAMP_WIDTH-1 downto 0);

        -- load from until memory
        nxt.last_level_phi := until_rdData(0);
        nxt.m_rising_phi   := until_rdData(TIMESTAMP_WIDTH + 2 - 1 downto 2);
        nxt.m_falling_phi  := until_rdData(2*TIMESTAMP_WIDTH + 2 - 1 downto TIMESTAMP_WIDTH + 2);
        nxt.m_pre          := until_rdData(3*TIMESTAMP_WIDTH + 2 - 1 downto 2*TIMESTAMP_WIDTH + 2);
        nxt.m_processed    := until_rdData(4*TIMESTAMP_WIDTH + 2 - 1 downto 3*TIMESTAMP_WIDTH + 2);

        pt_data_memory_addr <= this.command.op2.value(pt_data_memory_addr'length - 1 downto 0);
----------------------------------------------------------------------------

--        nxt.state := LOAD_OP2;

        case this.command.command is
          when OP_FT_UNTIL | OP_FT_AND =>
            nxt.state := LOAD_OP2;
          when others =>
            nxt.state := CALC;
        end case;

      when LOAD_OP2 =>
        case op2Type is
          when "10" =>                  -- read from queue
            nxt.op2      := queue_o.async;
            nxt.op2_sync := queue_o.sync;
            
          when "11" =>                  -- read from mtl mumonitor
            -- read from ptmtl mumonitor
            nxt.op2.head.value := pt_data_memory_data;
            nxt.op2.tail.value := pt_data_memory_data;
            nxt.op2.head.time  := timestamp;
            nxt.op2.tail.time  := timestamp;
            nxt.op2.empty      := '0';
            nxt.op2_sync       := "0" & pt_data_memory_data;
            
          when "00" =>                  -- read atomic
            nxt.op2.head.value := this.atomics(conv_to_integer(this.command.op2.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            nxt.op2.tail.value := this.atomics(conv_to_integer(this.command.op2.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            nxt.op2.head.time  := timestamp;
            nxt.op2.tail.time  := timestamp;
            nxt.op2.empty      := '0';
            nxt.op2_sync       := "0" & this.atomics(conv_to_integer(this.command.op2.value(log2c(ATOMICS_WIDTH)-1 downto 0)));
            
          when "01" =>                  -- immediate
            nxt.op2.head.value := this.command.op2.value(0);
            nxt.op2.tail.value := this.command.op2.value(0);
            nxt.op2.head.time  := timestamp;
            nxt.op2.tail.time  := timestamp;
            nxt.op2.empty      := '0';
            nxt.op2_sync       := "0" & this.command.op2.value(0);
            
          when others =>
            null;
        end case;

        if this.time = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then
          nxt.op2.head.value := '0';
          nxt.op2.head.value := '0';
        end if;

        nxt.queue_number := this.programCounter(nxt.queue_number'length - 1 downto 0) - 1;
        nxt.state        := CALC;

        -- edge detection for until operator
        if this.last_level_phi = '0' and this.op1.head.value = '1' and this.op1.empty = '0' then  -- rising edge
          nxt.m_rising_phi := this.op1.head.time - 1;
          nxt.m_pre        := (others => '0');
        end if;

        nxt.until_postpone := '0';

        if this.last_level_phi = '1' and this.op1.head.value = '0' and nxt.op2.head.value = '1' and this.op1.empty = '0' and nxt.op2.empty = '0'then  -- falling edge
          nxt.until_postpone := '1';
          nxt.m_falling_phi  := this.op1.head.time;
        end if;

        if this.op1.empty = '0' and this.op1.head.time >= this.m_processed then
          nxt.until_op1_ready := '1';
        else
          nxt.until_op1_ready := '0';
        end if;

        -- switch to determine next state
--        case this.command.command is
--          when OP_SINCE |
--            OP_BOXDOT |
--            OP_DIAMONDDOT =>
--            nxt.state := UPDATE_LIST;
--          when others =>
--            nxt.state := CALC;
--        end case;

      when CALC =>
        nxt.new_result := '0';
        nxt.update_q1  := NO;
        nxt.update_q2  := NO;
        nxt.state      := WRITE_BACK;

        case this.command.command is
          when OP_LOAD =>
            if this.op1.empty = '0' then
              nxt.new_result := '1';
              nxt.result     := this.op1.head;
              nxt.update_q1  := DEQUEUE;
            end if;

            nxt.result_sync := this.op1_sync;
            
          when OP_END_SEQUENCE =>
            if this.op1.empty = '0' then
              nxt.violated   := not this.op1.head.value;
              nxt.async_out  := this.op1.head;
              nxt.new_result := '1';
              nxt.update_q1  := DEQUEUE;
            else
              nxt.violated := '0';
            end if;

            nxt.result_sync_out := this.op1_sync;
            nxt.sync_out        := this.op1_sync;
            finish              <= '1';
            
          when OP_FT_BOXBOX |
            OP_FT_DIAMONDDIAMOND |
            OP_FT_BOXDOT |
            OP_FT_DIAMONDDOT =>

            nxt.box_dot_state := NONE;
            nxt.state         := CALC_BOX_DOT;

            if this.op1.empty = '0' then
              if this.last_level = '0' and this.op1.head.value = '1' then  -- rising edge
                nxt.m_rising := this.m_tau;
              end if;
              nxt.last_level := this.op1.head.value;

              if this.op1.head.value = '1' then
                nxt.box_dot_state := BD1;
              else
                nxt.box_dot_state := BD2;
              end if;

              nxt.update_q1 := DEQUEUE;
              
            end if;

            case this.command.command is
              when OP_FT_BOXBOX =>
                case this.op1_sync is
                  when FT_FALSE =>
                    nxt.result_sync := FT_FALSE;
                  when FT_TRUE =>
                    nxt.result_sync := FT_MAYBE;
                  when FT_MAYBE =>
                    nxt.result_sync := FT_MAYBE;
                  when others =>
                    null;
                end case;
                
              when OP_FT_DIAMONDDIAMOND =>
                case this.op1_sync is
                  when FT_FALSE =>
                    nxt.result_sync := FT_MAYBE;
                  when FT_TRUE =>
                    nxt.result_sync := FT_TRUE;
                  when FT_MAYBE =>
                    nxt.result_sync := FT_MAYBE;
                  when others =>
                    null;
                end case;
                
              when OP_FT_BOXDOT | OP_FT_DIAMONDDOT =>
                nxt.result_sync := FT_MAYBE;
                
              when others =>
                null;
            end case;
            
          when OP_FT_UNTIL =>
            nxt.until_state     := NONE;
            nxt.until_processed := '0';

            -- synchronize queues
            if this.until_op1_ready = '1' and this.op2.empty = '0' and this.op2.head.time >= this.m_processed then

              -- delete from queue ?
              if this.op1.head.time <= this.m_processed then
                nxt.update_q1 := DEQUEUE;
              end if;
              if this.op2.head.time <= this.m_processed then
                nxt.update_q2 := DEQUEUE;
              end if;

              nxt.last_level_phi  := this.op1.head.value;
              nxt.until_processed := '1';
              if this.until_postpone = '1' then
                nxt.last_level_phi := '0';
                nxt.result.time    := this.m_processed;
                nxt.result.value   := '1';
                nxt.new_result     := '1';
              end if;

              -- rising edge detection is done in previous state

              if this.op1.head.value = '1' or this.until_postpone = '1' then
                if this.op2.head.value = '1' then
                  nxt.until_state := U1;
                  
                else
                  if this.m_pre = std_logic_vector(to_unsigned(0, this.m_pre'length)) or this.m_pre + this.interval_length <= this.op1.head.time then
                    nxt.until_state  := U2;
                    nxt.result.value := '0';
                    nxt.new_result   := '1';
                  end if;
                end if;
              else
                nxt.until_state := U3;
              end if;
              
            end if;
            nxt.state := CALC_UNTIL;

            if this.op1_sync = FT_TRUE and this.op2_sync = FT_TRUE and (this.interval.min = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH))) then
              nxt.op1_sync := FT_TRUE;
            elsif this.op1_sync = FT_FALSE then
              nxt.result_sync := FT_FALSE;
            else
              nxt.result_sync := FT_MAYBE;
            end if;
            
          when OP_FT_NOT =>
            if this.op1.empty = '0' then
              nxt.update_q1  := DEQUEUE;
              nxt.new_result := '1';
            end if;

            nxt.result.time  := this.op1.head.time;
            nxt.result.value := not this.op1.head.value;

            case this.op1_sync is
              when FT_TRUE =>
                nxt.result_sync := FT_FALSE;
              when FT_FALSE =>
                nxt.result_sync := FT_TRUE;
              when FT_MAYBE =>
                nxt.result_sync := FT_MAYBE;
              when others =>
                null;
            end case;
            
          when OP_FT_AND =>
            if this.op1.head.value = '1' and this.op2.head.value = '1' and this.op1.empty = '0' and this.op2.empty = '0' then
              if this.op1.head.time = this.op2.head.time then
                nxt.result     := this.op1.head;
                nxt.new_result := '1';
                nxt.update_q1  := DEQUEUE;
                nxt.update_q2  := DEQUEUE;
              elsif this.op1.head.time > this.op2.head.time then
                nxt.result     := this.op2.head;
                nxt.new_result := '1';
                nxt.update_q2  := DEQUEUE;
              else
                nxt.result     := this.op1.head;
                nxt.new_result := '1';
                nxt.update_q1  := DEQUEUE;
              end if;
              
            elsif this.op1.head.value = '0' and this.op2.head.value = '0' and this.op1.empty = '0' and this.op2.empty = '0' then
              if this.op1.head.time < this.op2.head.time then
                b_does_not_hold := '1';
              else
                a_does_not_hold := '1';
              end if;
              
            elsif this.op1.head.value = '0' and this.op1.empty = '0' then
              a_does_not_hold := '1';
              
            elsif this.op2.head.value = '0' and this.op2.empty = '0' then
              b_does_not_hold := '1';

              --else
              --  nxt.maybe := '1';
              
            end if;

            if a_does_not_hold = '1' then
              nxt.result     := this.op1.head;
              nxt.new_result := '1';
              nxt.update_q1  := DEQUEUE;
              if this.op2.empty = '0' and this.op1.head.time >= this.op2.head.time then
                if this.op2.tail.time <= this.op1.head.time then
                  nxt.update_q2 := FLUSH;
                else
                  nxt.update_q2 := DEQUEUE;
                end if;
              end if;
            end if;

            if b_does_not_hold = '1' then
              nxt.result                                     := this.op2.head;
              nxt.new_result                                 := '1';
              nxt.update_q2                                  := DEQUEUE;
              if this.op1.empty = '0' and this.op1.head.time <= this.op2.head.time then
                if this.op1.tail.time <= this.op2.head.time then
                  nxt.update_q1 := FLUSH;
                else
                  nxt.update_q1 := DEQUEUE;
                end if;
              end if;
            end if;

             v_op_sync := this.op1_sync & this.op2_sync;
            --case (this.op1_sync & this.op2_sync) is
            case(v_op_sync) is     
              when FT_TRUE & FT_TRUE =>
                nxt.result_sync := FT_TRUE;
              when FT_TRUE & FT_FALSE =>
                nxt.result_sync := FT_FALSE;
              when FT_FALSE & FT_TRUE =>
                nxt.result_sync := FT_FALSE;
              when FT_FALSE & FT_FALSE =>
                nxt.result_sync := FT_FALSE;
              when FT_TRUE & FT_MAYBE =>
                nxt.result_sync := FT_MAYBE;
              when FT_MAYBE & FT_TRUE =>
                nxt.result_sync := FT_MAYBE;
              when FT_FALSE & FT_MAYBE =>
                nxt.result_sync := FT_FALSE;
              when FT_MAYBE & FT_FALSE =>
                nxt.result_sync := FT_FALSE;
              when FT_MAYBE & FT_MAYBE =>
                nxt.result_sync := FT_MAYBE;
              when others =>
                null;
            end case;
            
          when OP_FT_IMPLICIT =>
            if this.op1.head.value = '1' and this.op1.empty = '0' then
              if this.op2.empty = '0' then
                nxt.result     := this.op2.head;
                nxt.new_result := '1';
                if this.op1.head.time = this.op2.head.time then
                  nxt.update_q1 := DEQUEUE;
                  nxt.update_q2 := DEQUEUE;
                elsif this.op1.head.time > this.op2.head.time then
                  nxt.result.time := this.op2.head.time;
                  nxt.update_q2   := DEQUEUE;
                else
                  nxt.result.time := this.op1.head.time;
                  nxt.update_q1   := DEQUEUE;
                end if;
              end if;
            elsif this.op1.empty = '0' then
              nxt.result     := this.op1.head;
              nxt.new_result := '1';
              nxt.update_q1  := DEQUEUE;
              if this.op2.empty = '0' then
                if this.op2.tail.time <= this.op1.head.time then
                  nxt.update_q2 := FLUSH;
                else
                  nxt.update_q2 := DEQUEUE;
                end if;
              end if;
            end if;

            v_op_sync := this.op1_sync & this.op2_sync;
            --case (this.op1_sync & this.op2_sync) is
            case (v_op_sync) is     
              when FT_TRUE & FT_TRUE =>
                nxt.result_sync := FT_TRUE;
              when FT_TRUE & FT_FALSE =>
                nxt.result_sync := FT_FALSE;
              when FT_FALSE & FT_TRUE =>
                nxt.result_sync := FT_TRUE;
              when FT_FALSE & FT_FALSE =>
                nxt.result_sync := FT_TRUE;
              when FT_TRUE & FT_MAYBE =>
                nxt.result_sync := FT_MAYBE;
              when FT_MAYBE & FT_TRUE =>
                nxt.result_sync := FT_TRUE;
              when FT_FALSE & FT_MAYBE =>
                nxt.result_sync := FT_TRUE;
              when FT_MAYBE & FT_FALSE =>
                nxt.result_sync := FT_MAYBE;
              when FT_MAYBE & FT_MAYBE =>
                nxt.result_sync := FT_MAYBE;
              when others =>
                null;
            end case;
            
          when others =>
            null;
        end case;
        
      when CALC_BOX_DOT =>
        case this.box_dot_state is
          when NONE =>
            null;
          when BD1 =>
            if this.m_rising <= this.head_time_interval_max and this.time_greater_interval_max = '1' then  -- valid (now feasible)
              nxt.new_result   := '1';
              nxt.result.value := '1';
              nxt.m_tau        := this.op1.head.time + 1;
              nxt.result.time  := this.head_time_interval_max;
              --else
              --  nxt.maybe := '1';
              box_write        <= '1';
            end if;
            
          when BD2 =>
            nxt.result.value := '0';
            nxt.new_result   := '1';
            nxt.m_tau        := this.op1.head.time + 1;
            nxt.result.time  := this.op1.head.time;
            box_write        <= '1';
        end case;

        case this.command.command is
          when OP_FT_BOXDOT |
            OP_FT_DIAMONDDOT =>
            if nxt.result.time >= this.interval.min then
              nxt.result.time := nxt.result.time - this.interval.min;
            else
              nxt.new_result := '0';
            end if;
          when others =>
            null;
        end case;

        nxt.state := WRITE_BACK;
        
      when CALC_UNTIL =>
        case this.until_state is
          when NONE =>
            null;
            
          when U1 =>
            if this.m_rising_phi + this.interval.min < this.m_processed then
              nxt.m_pre        := this.m_processed;
              nxt.result.time  := this.m_processed - this.interval.min;
              nxt.result.value := '1';
              nxt.new_result   := '1';
            else
              if this.m_falling_phi /= std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then
                nxt.result.time  := this.m_falling_phi;
                nxt.result.value := '0';
                nxt.new_result   := '1';
              end if;
            end if;
            
          when U2 =>
            if this.m_rising_phi > this.m_processed - this.interval.max or this.m_processed < this.interval.max then  -- find max
              nxt.result.time := this.m_rising_phi;
            else
              nxt.result.time := this.m_processed - this.interval.max;
            end if;
            
          when U3 =>
            if this.until_postpone = '0' then
              nxt.m_falling_phi := (others => '0');
            end if;
            nxt.new_result  := '1';
            nxt.result.time := this.m_processed;
            if this.interval.min = std_logic_vector(to_unsigned(0, TIMESTAMP_WIDTH)) then
              nxt.result.value := this.op2.head.value;
            else
              nxt.result.value := '0';
            end if;
        end case;

        if this.until_processed = '1' then
          nxt.m_processed := this.m_processed + 1;
          until_write     <= '1';
        end if;

        nxt.state := WRITE_BACK;
        
      when WRITE_BACK =>
        -- switch to invert input signal
        case this.command.command is
          when OP_FT_DIAMONDDIAMOND |
            OP_FT_DIAMONDDOT =>
            queue_i.data.value <= not this.result.value;
          when others =>
            null;
        end case;

        queue_i.add <= this.new_result;

        nxt.queue_number          := this.command.op1.value(nxt.queue_number'length - 1 downto 0);
        nxt.state                 := UPDATE_Q1;
        queue_i.sync_result_write <= '1';

        -- only for simulation
        subformula_ram(to_integer(unsigned(this.programCounter))-1).async_maybe <= not this.new_result;
        if this.new_result = '1' then
          subformula_ram(to_integer(unsigned(this.programCounter))-1).async_result <= this.result;
          case this.command.command is
            when OP_FT_DIAMONDDIAMOND |
              OP_FT_DIAMONDDOT =>
              subformula_ram(to_integer(unsigned(this.programCounter))-1).async_result.value <= not this.result.value;
            when others =>
              null;
          end case;
        end if;
        subformula_ram(to_integer(unsigned(this.programCounter))-1).sync_result <= this.result_sync;
        
      when UPDATE_Q1 =>
        if this.command.op1.is_memory_addr = '1' and this.command.op1.is_immediate = '0' then
          case this.update_q1 is
            when DEQUEUE =>
              queue_i.delete_head <= '1';
            when FLUSH =>
              queue_i.flush <= '1';
            when others =>
              null;
          end case;

          if this.new_result = '1' then
            queue_i.update_input_level <= '1';
          end if;
          
        end if;

        nxt.queue_number := this.command.op2.value(nxt.queue_number'length - 1 downto 0);

--        nxt.state := UPDATE_Q2;
        case this.command.command is
          when OP_FT_UNTIL |
            OP_FT_AND =>
            nxt.state := UPDATE_Q2;
--PAMO: das macht keinen sinn hier im others zweig noch eine unterscheidung zu haben
-- zweiten others zweig weg optimieren
          when others =>
            case this.command.command is
              when OP_END_SEQUENCE =>
                nxt.state               := IDLE;
                nxt.forward_data_memory := '1';
              when others =>
                nxt.state := FETCH;
            end case;
        end case;
        
      when UPDATE_Q2 =>
        if this.command.op2.is_memory_addr = '1' and this.command.op2.is_immediate = '0' then
          case this.update_q2 is
            when DEQUEUE =>
              queue_i.delete_head <= '1';
            when FLUSH =>
              queue_i.flush <= '1';
            when others =>
              null;
          end case;

          if this.new_result = '1' then
            queue_i.update_input_level <= '1';
          end if;
          
        end if;

        case this.command.command is
          when OP_END_SEQUENCE =>
            nxt.state               := IDLE;
            nxt.forward_data_memory := '1';
          when others =>
            nxt.state := FETCH;
        end case;

      when others =>
        null;
    end case;

    -- precalculations
    nxt.interval_length := this.interval.max - this.interval.min;

   -- if this.forward_data_memory = '1' then
   --   queue_i.number <= data_memory_addr;
   -- else
      queue_i.number <= nxt.queue_number;
   -- end if;
    box_wrData   <= nxt.m_tau & this.m_rising & this.last_level;
    until_wrData <= nxt.m_processed & nxt.m_pre & nxt.m_falling_phi & this.m_rising_phi & '0' & this.last_level_phi;

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
