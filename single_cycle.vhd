
---------------------------------------------------------------------------
-- single_cycle_core.vhd - A Single-Cycle Processor Implementation
--
-- Notes : 
--
-- See single_cycle_core.pdf for the block diagram of this single
-- cycle processor core.
--
-- Instruction Set Architecture (ISA) for the single-cycle-core:
--   Each instruction is 16-bit wide, with four 4-bit fields.
--
--     noop      
--        # no operation or to signal end of program
--        # format:  | opcode = 0 |  0   |  0   |   0    | 
--
--     load  rt, rs, offset     
--        # load data at memory location (rs + offset) into rt
--        # format:  | opcode = 1 |  rs  |  rt  | offset |
--
--     store rt, rs, offset
--        # store data rt into memory location (rs + offset)
--        # format:  | opcode = 3 |  rs  |  rt  | offset |
--
--     add   rd, rs, rt
--        # rd <- rs + rt
--        # format:  | opcode = 8 |  rs  |  rt  |   rd   |
--
--
-- Copyright (C) 2006 by Lih Wen Koh (lwkoh@cse.unsw.edu.au)
-- All Rights Reserved. 
--
-- The single-cycle processor core is provided AS IS, with no warranty of 
-- any kind, express or implied. The user of the program accepts full 
-- responsibility for the application of the program and the use of any 
-- results. This work may be downloaded, compiled, executed, copied, and 
-- modified solely for nonprofit, educational, noncommercial research, and 
-- noncommercial scholarship purposes provided that this notice in its 
-- entirety accompanies all copies. Copies of the modified software can be 
-- delivered to persons who use it solely for nonprofit, educational, 
-- noncommercial research, and noncommercial scholarship purposes provided 
-- that this notice in its entirety accompanies all copies.
--
---------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity single_cycle_core is
    port ( reset  : in  std_logic;
           clk    : in  std_logic );
end single_cycle_core;

architecture structural of single_cycle_core is

component program_counter is
    port ( reset    : in  std_logic;
           clk      : in  std_logic;
           addr_in  : in  std_logic_vector(3 downto 0);
           addr_out : out std_logic_vector(3 downto 0) );
end component;

component instruction_memory is
    port ( reset    : in  std_logic;
           clk      : in  std_logic;
           addr_in  : in  std_logic_vector(3 downto 0);
           IF_Id : out std_logic_vector(19 downto 0) );
end component;

component sign_extend_4to16 is
    port ( data_in  : in  std_logic_vector(3 downto 0);
           data_out : out std_logic_vector(15 downto 0) );
end component;

component mux_2to1_4b is
    port ( mux_select : in  std_logic;
           data_a     : in  std_logic_vector(3 downto 0);
           data_b     : in  std_logic_vector(3 downto 0);
           data_out   : out std_logic_vector(3 downto 0) );
end component;

component mux_2to1_16b is
    port ( mux_select : in  std_logic;
           data_a     : in  std_logic_vector(15 downto 0);
           data_b     : in  std_logic_vector(15 downto 0);
           data_out   : out std_logic_vector(15 downto 0) );
end component;

component control_unit is
     port ( IF_ID    : in  std_logic_vector(19 downto 0);
           clk        : in std_logic;
           reg_write  : out std_logic;
           ID_EX      : out std_logic_vector(60 downto 56));
end component;

component register_file is
   port ( reset           : in  std_logic;
           clk             : in  std_logic;
           IF_ID           : in std_logic_vector(19 downto 0);
           write_enable    : in  std_logic;
           write_data      : in  std_logic_vector(15 downto 0);
           ID_EX            : out std_logic_vector(60 downto 0);
           regb             : out std_logic_vector(15 downto 0) );
 
end component;

component adder_4b is
    port ( src_a     : in  std_logic_vector(3 downto 0);
           src_b     : in  std_logic_vector(3 downto 0);
           sum       : out std_logic_vector(3 downto 0);
           carry_out : out std_logic );
end component;

component adder_16b is
--    port ( src_a     : in  std_logic_vector(15 downto 0);
--           src_b     : in  std_logic_vector(15 downto 0);
--           sum       : out std_logic_vector(15 downto 0);
--           carry_out : out std_logic );

--    Port ( ALUOperation: in std_logic_vector(3 downto 0);
--           src_a : in STD_LOGIC_VECTOR (15 downto 0);
--           src_b : in STD_LOGIC_VECTOR (15 downto 0);
--           result : out STD_LOGIC_VECTOR (15 downto 0));

    port ( clk     : in std_logic;
            ID_EX   : in std_logic_vector(60 downto 0);
            EX_MEM  : out std_logic_vector(18 downto 0) );
end component;

component data_memory is
    port ( reset        : in  std_logic;
           clk          : in  std_logic;
           write_enable : in  std_logic;
           write_data   : in  std_logic_vector(15 downto 0);
           addr_in      : in  std_logic_vector(3 downto 0);
           data_out     : out std_logic_vector(15 downto 0) );
end component;
component bne is 
     port ( ALUOperation: in  std_logic_vector(3 downto 0);
        condition: in std_logic_vector(15 downto 0);
        where: in STD_LOGIC_VECTOR (3 downto 0);
        current: in std_logic_vector(3 downto 0);
        breach:  out std_logic;
        output: out STD_LOGIC_VECTOR (3 downto 0));
end component;
--component SPupdate is
--    port ( 
--        reset           :in std_logic;
--        op              : in  std_logic_vector(3 downto 0);

--        SPcontrol: in std_logic;
--        currSP: in integer range 0 to 15; 
--        nextSP: out integer range 0 to 15);
--end component;

--component SP_counter is
--Port (
--        reset:  in std_logic;
--        op:     in std_logic_vector(3 downto 0);
--        nextSP: in integer range 0 to 15;
--        currSP: out integer range 0 to 15 );
--end component;      

signal sig_next_pc              : std_logic_vector(3 downto 0);
signal sig_curr_pc              : std_logic_vector(3 downto 0);
signal sig_one_4b               : std_logic_vector(3 downto 0);
signal sig_pc_carry_out         : std_logic;
signal sig_insn                 : std_logic_vector(15 downto 0);
signal sig_sign_extended_offset : std_logic_vector(15 downto 0);
signal sig_reg_dst              : std_logic;
signal sig_reg_write            : std_logic;
signal sig_alu_src              : std_logic;
signal sig_mem_write            : std_logic;
signal sig_mem_to_reg           : std_logic;
signal sig_write_register       : std_logic_vector(3 downto 0);
signal sig_write_data           : std_logic_vector(15 downto 0);
signal sig_read_data_a          : std_logic_vector(15 downto 0);
signal sig_read_data_b          : std_logic_vector(15 downto 0);
signal sig_alu_src_b            : std_logic_vector(15 downto 0);
signal sig_alu_result           : std_logic_vector(15 downto 0); 
signal sig_alu_carry_out        : std_logic;
signal sig_data_mem_out         : std_logic_vector(15 downto 0);
signal potential       : std_logic_vector(3 downto 0);
signal bnePotential       : std_logic_vector(3 downto 0);

signal pcMu            :std_logic;
signal currSP           :integer range 0 to 15;
signal nextSP           :integer range 0 to 15;
signal SPcontrol        : std_logic;    --push or pop operation

-- pipeline registers
-- 16 bit instruction, 4 bit address 
signal IF_ID    :std_logic_vector(19 downto 0);
signal ID_EX    :std_logic_vector(60 downto 0);
signal EX_MEM   :std_logic_vector(18 downto 0);

begin

    -- This is the IF stage

    sig_one_4b <= "0001";

    pc : program_counter
    port map ( reset    => reset,
               clk      => clk,
               addr_in  => sig_next_pc,
               addr_out => sig_curr_pc ); 
         
    -- this function will update the sp for each insturction       
--    sp: SP_counter
--    port map(
--        reset => reset,
--        op => sig_curr_pc, 
--        nextSP => nextSP,
--        currSP => currSP
--    );
    

    next_pc : adder_4b 
    port map ( src_a     => sig_curr_pc, 
               src_b     => sig_one_4b,
               sum       => potential,   
               carry_out => sig_pc_carry_out );
               
    insn_mem : instruction_memory 
    port map ( reset    => reset,
               clk      => clk,
               addr_in  => sig_curr_pc,
               IF_ID    => IF_ID );
    
--    insn_mem : instruction_memory 
--    port map ( reset    => reset,
--               clk      => clk,
--               addr_in  => sig_curr_pc,
--               insn_out => sig_insn );


    -- This is ID stage
    sign_extend : sign_extend_4to16 
    port map ( data_in  => sig_insn(3 downto 0),
               data_out => sig_sign_extended_offset );



      ctrl_unit : control_unit 
        port map ( IF_ID => IF_ID,
           clk =>clk,
           reg_write => sig_reg_write,
           ID_EX => ID_EX(60 downto 56));
--    ctrl_unit : control_unit 
--    port map ( opcode     => IF_ID(15 downto 12),
--               reg_dst    => sig_reg_dst,
--               reg_write  => sig_reg_write,
--               alu_src    => sig_alu_src,
--               mem_write  => sig_mem_write,
--               mem_to_reg => sig_mem_to_reg,
--               SPcontrol  => SPcontrol );
     
    -- this function will calculate the sp pointer based on the operation          
--    update: SPupdate
--    port map (
--            reset           => reset,
--            op              => sig_curr_pc,
--            SPcontrol       => SPcontrol,
--            currSP          => currSP,
--            nextSP          => nextSP);
              



    -- register file will access the stack using the sp pointer 
    -- if operation is add will read sp and sp+1
    -- otherwise will only ready sp
    mux_reg_dst : mux_2to1_4b 
    port map ( mux_select => sig_reg_dst,
               data_a     => sig_insn(7 downto 4),
               data_b     => sig_insn(3 downto 0),
               data_out   => sig_write_register );

--    reg_file : register_file 
--    port map ( reset           => reset, 
--               clk             => clk,
--               read_register_a => sig_insn(11 downto 8),
--               read_register_b => sig_insn(7 downto 4),
--               write_enable    => sig_reg_write,
--               write_register  => sig_write_register,
--               write_data      => sig_write_data,
--               read_data_a     => sig_read_data_a,
--               read_data_b     => sig_read_data_b );

    reg_file : register_file 
    port map ( reset           => reset, 
               clk             => clk,
               IF_ID           => IF_ID,
               write_enable    => sig_reg_write,            --  
               write_data      => sig_write_data,
               ID_EX           => ID_EX,
               regb           => sig_read_data_b );
    
    
    mux_alu_src : mux_2to1_16b 
    port map ( mux_select => ID_EX(58),
               data_a     => sig_read_data_b,
               data_b     => sig_sign_extended_offset,
               data_out   => ID_EX(31 downto 16));

               
--    alu: adder_16b
--        port map( ALUOperation => sig_insn(15 downto 12),
--        src_a => sig_read_data_a,
--        src_b => sig_alu_src_b,
--        result => sig_alu_result);

    alu: adder_16b
        port map( 
            clk => clk,
            ID_EX => ID_EX,
            EX_MEM => EX_MEM);
 
 -- implementing a different format operation     
 -- reading two input 
 -- reading the third int  
     bneF: bne
        port map(ALUOperation => sig_insn(15 downto 12),
        condition => sig_alu_result,
        where => sig_insn(3 downto 0),
        current => potential,
        breach  => pcMu,
        output => bnePotential);
        
    mux_pc : mux_2to1_4b 
    port map ( mux_select => pcMu,
               data_a     => potential,
               data_b     => bnePotential,
               data_out   => sig_next_pc );
    
    data_mem : data_memory 
    port map ( reset        => reset,
               clk          => clk,
               write_enable => sig_mem_write,
               write_data   => sig_read_data_b,
               addr_in      => sig_alu_result(3 downto 0),
               data_out     => sig_data_mem_out );
               
    mux_mem_to_reg : mux_2to1_16b 
    port map ( mux_select => sig_mem_to_reg,
               data_a     => sig_alu_result,
               data_b     => sig_data_mem_out,
               data_out   => sig_write_data );

end structural;
