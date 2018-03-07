library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity datapath is
port(clk : in std_logic;
	reset : in std_logic;
	pw : in std_logic;
	mw : in std_logic;
	iord : in std_logic_vector(1 downto 0);
	iw : in std_logic;
	dw : in std_logic;
	memcode : in unsigned(2 downto 0);
	shiftcode : in std_logic_vector(1 downto 0);
	shiftamtslct : in std_logic_vector(1 downto 0);
	rsrc : in std_logic_vector(1 downto 0);
	wdest : in std_logic_vector(1 downto 0);
	rw : in std_logic;
	m2r : in std_logic;
	sorm : in std_logic;
	aw : in std_logic;
	bw : in std_logic;
	asrc1 : in std_logic_vector(1 downto 0);
	asrc2 : in std_logic_vector(1 downto 0);
	opcode : in std_logic_vector(3 downto 0);
	fset : in std_logic;
	rew : in std_logic;
	outp : out std_logic
);
end entity;

architecture behav of datapath is

component shifter
port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		shiftamt : in unsigned(4 downto 0);
		result : out signed(31 downto 0);
		carry : out std_logic
);
end component;

component alu
port(	op1 : in signed(31 downto 0);
		op2 : in signed(31 downto 0);
		opcode : in std_logic_vector(3 downto 0);
		carry : in std_logic;
		result : out signed(31 downto 0);
		nflag : out std_logic;
		zflag : out std_logic;
		vflag : out std_logic;
		cflag : out std_logic
);
end component;

component mult
port( op1 : in signed(31 downto 0);
      op2 : in signed(31 downto 0);
      result : out signed(31 downto 0)  
);
end component;

component regfile
port(wr_data: in signed(31 downto 0);
    rd_addr1 : in unsigned(3 downto 0);
    rd_addr2 : in unsigned(3 downto 0);
    wr_addr : in unsigned(3 downto 0);
    clk : in std_logic;
    reset : in std_logic;
    wr_enable : in std_logic;
    rd_data1 : out signed(31 downto 0);
    rd_data2 : out signed(31 downto 0)
);
end component;

component mempro
port(inp_pro : in signed(31 downto 0);
    inp_mem : in signed(31 downto 0);
    opcode : in unsigned(2 downto 0);
    offset_in_word : in unsigned(1 downto 0);
    out_pro : out signed(31 downto 0);
    out_mem : out signed(31 downto 0)
);
end component;

component memory
port(wr_data: in signed(31 downto 0);
    addr : in unsigned(31 downto 0);
    clk : in std_logic;
    wr_enable : in std_logic;                            ---------allowing reads forever------------------
    rd_data : out signed(31 downto 0)  
);
end component;

signal pc : signed(31 downto 0) := "00000000000000000000000000000000";
signal ir : signed(31 downto 0);
signal dr : signed(31 downto 0);
signal a : signed(31 downto 0);
signal b : signed(31 downto 0);
signal res : signed(31 downto 0);
signal nflag : std_logic;
signal zflag : std_logic;
signal cflag : std_logic;
signal vflag : std_logic;

signal zero: signed(31 downto 0) := "00000000000000000000000000000000";
signal two: signed(1 downto 0) := "10";

signal memaddr : unsigned(31 downto 0);
signal mempro2mem : signed(31 downto 0); 
signal mem2irdr : signed(31 downto 0);
signal rdaddr1 : unsigned(3 downto 0);
signal wraddr : unsigned(3 downto 0);
signal rfread1 : signed(31 downto 0);
signal rfread2 : signed(31 downto 0);
signal mempro2rf : signed(31 downto 0);
signal mul2b : signed(31 downto 0);
signal shiftamount : unsigned(4 downto 0);
signal shift2b : signed(31 downto 0);
signal shiftcarry : std_logic;
signal binp : signed(31 downto 0);
signal ainp1 : signed(31 downto 0);
signal ainp2 : signed(31 downto 0);
signal alu2res : signed(31 downto 0);
signal alucarry : std_logic;
signal alun, aluc, aluz, aluv : std_logic;
signal shiftamt2 : unsigned(5 downto 0);

begin

outp <= '1';

shiftamt2 <= unsigned(ir(11 downto 8)*two);

with asrc1 select ainp1 <=
	pc when "00",
	a when "01",
	zero when others;
	
with asrc2 select ainp2 <=
	b when "00",
	"00000000000000000000000000000100" when "01",
	zero(31 downto 12)&ir(11 downto 0) when "10",
	zero(31 downto 26)&ir(23 downto 0)&"00" when others;

with sorm select binp <=
	shift2b when '1',
	mul2b when others;

with shiftamtslct select shiftamount <=
	"00000" when "00",
	unsigned(ir(11 downto 7)) when "01",
	shiftamt2(4 downto 0) when "10",
	unsigned(rfread1(4 downto 0)) when others;	

with iord select memaddr <= 
	unsigned(pc) when "00",
	unsigned(res) when "01",
	unsigned(a) when others;
	
with rsrc select rdaddr1 <= 
	unsigned(ir(19 downto 16)) when "00",
	unsigned(ir(15 downto 12)) when "01",
	unsigned(ir(11 downto 8)) when others;
	
with wdest select wraddr <=
	unsigned(ir(19 downto 16)) when "00",
	unsigned(ir(15 downto 12)) when "01",
	"1110" when others;
	
mem : memory port map (
	addr => memaddr,
	wr_data => mempro2mem,
	clk => clk,
	wr_enable => mw,
	rd_data => mem2irdr
	);
	
regf : regfile port map (
	rd_addr1 => rdaddr1,
	rd_addr2 => unsigned(ir(3 downto 0)),
	wr_addr => wraddr,
	clk => clk,
	reset => reset,
	wr_enable => rw,
	rd_data1 => rfread1,
	rd_data2 => rfread2,
	wr_data => mempro2rf
	);
	
mul : mult port map (
	op1 => rfread1,
	op2 => rfread2,
	result => mul2b
	);
	
shift: shifter port map (
	op => rfread2,
	shiftcode => shiftcode,
	shiftamt => shiftamount,
	result => shift2b, 
	carry => shiftcarry
	);	
	
memproc : mempro port map (
	inp_pro => b,
	inp_mem => dr,
	out_mem => mempro2mem,
	out_pro => mempro2rf,
	offset_in_word => unsigned(res(1 downto 0)),
	opcode => memcode
	);	
	
arithlog : alu port map (
	op1 => ainp1,
	op2 => ainp2,
	opcode => opcode,
	carry => cflag,
	result => alu2res,
	nflag => alun,
	zflag => aluz,
	cflag => aluc,
	vflag => aluv
	);
	
process(iw)
begin
	case iw is 
		when '1' => ir <= mem2irdr;
		when others => null;
	end case;
end process;

process(dw)
begin
	case dw is 
		when '1' => dr <= mem2irdr;
		when others => null;
	end case;
end process;

process(aw)
begin
	case aw is 
		when '1' => a <= rfread1;
		when others => null;
	end case;
end process;

process(bw)
begin
	case bw is 
		when '1' => b <= binp;
		when others => null;
	end case;
end process;

process(fset)
begin
	case fset is 
		when '1' => nflag <= alun; zflag <= aluz;
					if ((opcode = "0000") or (opcode = "0001") or (opcode = "1100") or (opcode = "1110") or (opcode = "1111") or (opcode = "1101")) then
						cflag <= shiftcarry; else cflag <= aluc; end if;
					vflag <= aluv;				
		when others => null;
	end case;
end process;

process(rew)
begin
	case rew is 
		when '1' => res <= alu2res;
		when others => null;
	end case;
end process;		
		
end architecture;

--------------------------------------sas----------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity alu is 

port(	op1 : in signed(31 downto 0);
		op2 : in signed(31 downto 0);
		opcode : in std_logic_vector(3 downto 0);
		carry : in std_logic;
		result : out signed(31 downto 0);
		nflag : out std_logic;
		zflag : out std_logic;
		vflag : out std_logic;
		cflag : out std_logic
);
end entity;

architecture behav of alu is

constant zero: signed(31 downto 0) := "00000000000000000000000000000000";
signal res : signed(31 downto 0);
signal c31, c32 : std_logic;
signal c : signed(0 downto 0);

begin

with carry select c <=
	"0" when '0',
	"1" when others;

process(op1, op2, opcode)
begin
    case opcode is
        when "0000" => res <= op1 and op2; 
        when "0001" => res <= op1 xor op2; 
        when "0010" => res <= op1 + not op2 + 1; 
        when "0011" => res <= not op1 + op2 + 1; 
        when "0100" => res <= op1 + op2; 
        when "0101" => res <= op1 + op2 + c; 
        when "0110" => res <= op1 + not op2 + c; 
        when "0111" => res <= not op1 + op2 + c;
        when "1000" => res <= op1 and op2;
        when "1001" => res <= op1 xor op2;
        when "1010" => res <= op1 + not op2 + 1;
        when "1011" => res <= op1 + op2;
        when "1100" => res <= op1 or op2; 
        when "1101" => res <= op2;
        when "1110" => res <= op1 and (not op2); 
        when "1111" => res <= not op2;
        when others => null;
    end case;
end process;

result <= res;
nflag <= res(31);
with res select zflag <=
    '1' when "00000000000000000000000000000000",
    '0' when others;
    
process(op1, op2, opcode)
begin
	case opcode is 
		when "1011"|"1010"|"0010"|"0011"|"0100"|"0101"|"0110"|"0111" => 
            cflag <= (op1(31) and op2(31)) or ((op1(31) or op2(31)) and (op1(31) xor op2(31) xor res(31)));
            if (op1(31) = op2(31) and op1(31) = not res(31)) then vflag <= '1'; else vflag <= '0'; end if;
		when others => null;
	end case;
end process;

end architecture;

-----------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity shifter is 

port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		shiftamt : in unsigned(4 downto 0);
		result : out signed(31 downto 0);
		carry : out std_logic
);
end entity;

architecture behav of shifter is

component shifter1 
port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end component;

component shifter2 
port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end component;

component shifter4 
port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end component;

component shifter8
port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end component;

component shifter16 
port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end component;

constant zero: signed(31 downto 0) := "00000000000000000000000000000000";
constant ones : signed(31 downto 0) := "11111111111111111111111111111111"; 
signal temp1, temp2, temp3, temp4 : signed(31 downto 0);
signal c1, c2, c3, c4: std_logic;

begin

s1: shifter1 port map (op, shiftcode, shiftamt(0), temp1, c1);
s2: shifter2 port map (temp1, shiftcode, shiftamt(1), c1, temp2, c2);
s4: shifter4 port map (temp2, shiftcode, shiftamt(2), c2, temp3, c3);
s8: shifter8 port map (temp3, shiftcode, shiftamt(3), c3, temp4, c4);
s16: shifter16 port map (temp4, shiftcode, shiftamt(4), c4, result, carry);
		
end architecture;

-----------------------------------------sas------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity shifter1 is 

port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end entity;

architecture behav of shifter1 is

constant zero : signed(31 downto 0) := "00000000000000000000000000000000";
constant ones : signed(31 downto 0) := "11111111111111111111111111111111"; 
signal ext : signed(31 downto 0);

begin

process(op, shiftcode, enable)
begin
	if (enable = '1') then
		case shiftcode is 
		when "00"=> carry <= op(31); result <= op(30 downto 0)&zero(0 downto 0);
		when "01"=> carry <= op(0); result <= zero(0 downto 0)&op(31 downto 1); 
		when "10"=> if op(31) = '1' then result <= ones(0 downto 0)&op(31 downto 1); else result <= zero(0 downto 0)&op(31 downto 1); end if;
		when "11"=> carry <= op(0); result <= op(0 downto 0)&op(31 downto 1);
		when others => null;
		end case;
	else
		carry <= '0';
		result <= op;
	end if;
end process;
		
end architecture;

------------------------------sas------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity shifter2 is 

port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end entity;

architecture behav of shifter2 is

constant zero : signed(31 downto 0) := "00000000000000000000000000000000";
constant ones : signed(31 downto 0) := "11111111111111111111111111111111"; 
signal ext : signed(31 downto 0);

begin

process(op, shiftcode, enable, carryin)
begin
	if (enable = '1') then
		case shiftcode is 
		when "00"=> carry <= op(30); result <= op(29 downto 0)&zero(1 downto 0);
		when "01"=> carry <= op(1); result <= zero(1 downto 0)&op(31 downto 2); 
		when "10"=> if op(31) = '1' then result <= ones(1 downto 0)&op(31 downto 2); else result <= zero(1 downto 0)&op(31 downto 2); end if;
		when "11"=> carry <= op(1); result <= op(1 downto 0)&op(31 downto 2);
		when others => null;
		end case;
	else
		carry <= carryin;
		result <= op;
	end if;
end process;
		
end architecture;

---------------------------------------sas------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity shifter4 is 

port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end entity;

architecture behav of shifter4 is

constant zero : signed(31 downto 0) := "00000000000000000000000000000000";
constant ones : signed(31 downto 0) := "11111111111111111111111111111111"; 
signal ext : signed(31 downto 0);

begin

process(op, shiftcode, enable, carryin)
begin
	if (enable = '1') then
		case shiftcode is 
		when "00"=> carry <= op(28); result <= op(27 downto 0)&zero(3 downto 0);
		when "01"=> carry <= op(3); result <= zero(3 downto 0)&op(31 downto 4); 
		when "10"=> if op(31) = '1' then result <= ones(3 downto 0)&op(31 downto 4); else result <= zero(3 downto 0)&op(31 downto 4); end if;
		when "11"=> carry <= op(3); result <= op(3 downto 0)&op(31 downto 4);
		when others => null;
		end case;
	else
		carry <= carryin;
		result <= op;
	end if;
end process;
		
end architecture;

---------------------------------------sas------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity shifter8 is 

port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end entity;

architecture behav of shifter8 is

constant zero: signed(31 downto 0) := "00000000000000000000000000000000";
constant ones : signed(31 downto 0) := "11111111111111111111111111111111"; 
signal ext : signed(31 downto 0);

begin

process(op, shiftcode, enable, carryin)
begin
	if (enable = '1') then
		case shiftcode is 
		when "00"=> carry <= op(24); result <= op(23 downto 0)&zero(7 downto 0);
		when "01"=> carry <= op(7); result <= zero(7 downto 0)&op(31 downto 8); 
		when "10"=> carry <= op(7); if op(31) = '1' then result <= ones(7 downto 0)&op(31 downto 8); else result <= zero(7 downto 0)&op(31 downto 8); end if;
		when "11"=> carry <= op(7); result <= op(7 downto 0)&op(31 downto 8);
		when others => null;
		end case;
	else
		carry <= carryin;
		result <= op;
	end if;
end process;
		
end architecture;

---------------------------------------sas------------------------------------------------




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity shifter16 is 

port(	op : in signed(31 downto 0);
		shiftcode : in std_logic_vector(1 downto 0);
		enable : in std_logic;
		carryin : in std_logic;
		result : out signed(31 downto 0);
		carry : out std_logic
);
end entity;

architecture behav of shifter16 is

constant zero: signed(31 downto 0) := "00000000000000000000000000000000";
constant ones : signed(31 downto 0) := "11111111111111111111111111111111"; 
signal ext : signed(31 downto 0);

begin

process(op, shiftcode, enable, carryin)
begin
	if (enable = '1') then
		case shiftcode is 
		when "00"=> carry <= op(16); result <= op(15 downto 0)&zero(15 downto 0);
		when "01"=> carry <= op(15); result <= zero(15 downto 0)&op(31 downto 16); 
		when "10"=> if op(31) = '1' then result <= ones(15 downto 0)&op(31 downto 16); else result <= zero(15 downto 0)&op(31 downto 16); end if;
		when "11"=> carry <= op(15); result <= op(15 downto 0)&op(31 downto 16);
		when others => null;
		end case;
	else
		carry <= carryin;
		result <= op;
	end if;
end process;
		
end architecture;

---------------------------------------sas------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mult is

port( op1 : in signed(31 downto 0);
      op2 : in signed(31 downto 0);
      result : out signed(31 downto 0)  
);
end entity;

architecture behav of mult is

signal int64 : signed(63 downto 0);

begin

int64 <= op1*op2;
result <= int64(31 downto 0);

end behav;

--------------------------------------------------sas-----------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity regfile is

port(wr_data: in signed(31 downto 0);
    rd_addr1 : in unsigned(3 downto 0);
    rd_addr2 : in unsigned(3 downto 0);
    wr_addr : in unsigned(3 downto 0);
    clk : in std_logic;
    reset : in std_logic;
    wr_enable : in std_logic;
    rd_data1 : out signed(31 downto 0);
    rd_data2 : out signed(31 downto 0)
);

end entity;

architecture behav of regfile is

type storage is array(15 downto 0) of signed(31 downto 0);

signal regs : storage;
constant zero: signed(31 downto 0) := "00000000000000000000000000000000";

begin

process(clk, wr_enable, wr_data, wr_addr, reset)
begin
    if (reset = '1') then
        regs(15) <= zero;
		regs(14) <= zero;
		regs(13) <= zero;
		regs(12) <= zero;
		regs(11) <= zero;
		regs(10) <= zero;
		regs(9) <= zero;
		regs(8) <= zero;
		regs(7) <= zero;
		regs(6) <= zero;
		regs(5) <= zero;
		regs(4) <= zero;
		regs(3) <= zero;
		regs(2) <= zero;
		regs(1) <= zero;
		regs(0) <= zero;
    end if;
    if (clk'event and clk = '1') then
        if (wr_enable = '1') then
            regs(to_integer(wr_addr)) <= wr_data;
        end if;
    end if;
end process;

rd_data1 <= regs(to_integer(rd_addr1));
rd_data2 <= regs(to_integer(rd_addr2));

end architecture;

--------------------------------------------sas---------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mempro is

port(inp_pro : in signed(31 downto 0);
    inp_mem : in signed(31 downto 0);
    opcode : in unsigned(2 downto 0);
    offset_in_word : in unsigned(1 downto 0);
    out_pro : out signed(31 downto 0);
    out_mem : out signed(31 downto 0)
);
end entity;

architecture behav of mempro is

constant ones : signed(31 downto 0) := "11111111111111111111111111111111";
constant zero: signed(31 downto 0) := "00000000000000000000000000000000";

begin
--- this process if for str
process(inp_pro, inp_mem, opcode, offset_in_word)
begin
	case opcode is 
        when "001" => out_mem <= inp_pro;
        when "011" => out_mem <= inp_pro(7 downto 0)&inp_pro(7 downto 0)&inp_pro(7 downto 0)&inp_pro(7 downto 0);
        when "101" => out_mem <= inp_pro(15 downto 0)&inp_pro(15 downto 0);
        when "000" => out_pro <= inp_mem;
        when "010" => if (offset_in_word = "00") then out_pro <= zero(23 downto 0)&inp_mem(7 downto 0);
                                    elsif (offset_in_word = "01") then out_pro <= zero(23 downto 0)&inp_mem(15 downto 8);
                                    elsif (offset_in_word = "10") then out_pro <= zero(23 downto 0)&inp_mem(23 downto 16);
                                    else out_pro <= zero(23 downto 0)&inp_mem(31 downto 24); end if;
        when "100" => if (offset_in_word = "00") then out_pro <= zero(15 downto 0)&inp_mem(15 downto 0);
                                    elsif (offset_in_word = "10") then out_pro <= zero(15 downto 0)&inp_mem(31 downto 16); end if;
                                    ----error if offset is not these values
        when "110" => if (offset_in_word = "00") then if inp_mem(7)='1' then
                                        out_pro <= ones(23 downto 0)&inp_mem(7 downto 0);
                                        else out_pro <= zero(23 downto 0)&inp_mem(7 downto 0); end if;
                                    elsif (offset_in_word = "01") then if inp_mem(15)='1' then
                                        out_pro <= ones(23 downto 0)&inp_mem(15 downto 8);
                                        else out_pro <= zero(23 downto 0)&inp_mem(15 downto 8); end if;
                                    elsif (offset_in_word = "10") then if inp_mem(23)='1' then
                                        out_pro <= ones(23 downto 0)&inp_mem(23 downto 16);
                                        else out_pro <= zero(23 downto 0)&inp_mem(23 downto 16); end if;
                                    else if inp_mem(7)='1' then
                                        out_pro <= ones(23 downto 0)&inp_mem(31 downto 24);
                                        else out_pro <= zero(23 downto 0)&inp_mem(31 downto 24); end if; end if;
        when "111" => if (offset_in_word = "00") then if inp_mem(15)='1' then
                                        out_pro <= ones(15 downto 0)&inp_mem(15 downto 0);
                                        else out_pro <= zero(15 downto 0)&inp_mem(15 downto 0); end if;
                                    elsif (offset_in_word = "10") then if inp_mem(31)='1' then
                                        out_pro <= ones(15 downto 0)&inp_mem(31 downto 16);
                                        else out_pro <= zero(15 downto 0)&inp_mem(31 downto 16); end if; end if;
                                    ---else error!
        when others => null;
	end case;
end process;

end architecture;

-- 000 ldr
-- 001 str
-- 010 ldrb
-- 011 strb
-- 100 ldrh
-- 101	strh
-- 110 ldrsb
-- 111	ldrsh

----------------------------------------------------------sas----------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity memory is

port(wr_data: in signed(31 downto 0);
    addr : in unsigned(31 downto 0);
    clk : in std_logic;
    wr_enable : in std_logic;                            ---------allowing reads forever------------------
    rd_data : out signed(31 downto 0)  
);
end entity;

architecture behav of memory is

type storage is array(63 downto 0) of signed(31 downto 0);
signal regs : storage;

begin

process(clk, wr_enable, wr_data, addr)
begin
    if (clk'event and clk = '1') then
        if (wr_enable = '1') then
            regs(to_integer(addr)) <= wr_data;
        end if;
    end if;
end process;

rd_data <= regs(to_integer(addr));

end architecture;

--------------------------------------------sas---------------------------------------------------------





