library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity datacontr is 
port(topclk : in std_logic;
	reset : in std_logic;
	aluout : out signed(31 downto 0)
);
end entity;

architecture behav of datacontr is

component datapath
port(clk : in std_logic;
	reset : in std_logic;
	pw : in std_logic;
	mw : in std_logic;
	iord : in std_logic_vector(1 downto 0);
	iw : in std_logic;
	dw : in std_logic;
	memcode : in unsigned(2 downto 0);
	shiftinpslct : in std_logic_vector(1 downto 0);
	shiftcode : in std_logic_vector(1 downto 0);
	shiftamtslct : in std_logic_vector(1 downto 0);
	rsrc : in std_logic_vector(1 downto 0);
	rsrc2 : in std_logic;
	wdest : in std_logic_vector(1 downto 0);
	rw : in std_logic;
	m2r : in std_logic_vector(1 downto 0);
	sorm : in std_logic;
	aw : in std_logic;
	bw : in std_logic;
	asrc1 : in std_logic_vector(1 downto 0);
	asrc2 : in std_logic_vector(1 downto 0);
	opcode : in std_logic_vector(3 downto 0);
	fset : in std_logic;
	rew : in std_logic;
	nflag : out std_logic;
	zflag : out std_logic;
	cflag : out std_logic;
	vflag : out std_logic;
	resout : out signed(31 downto 0);
	insout : out signed(31 downto 0)
);
end component;

component totalctrl
port(clk : in std_logic;
    reset : in std_logic;
	nflag : in std_logic;
	vflag : in std_logic;
	cflag : in std_logic;
	zflag : in std_logic;
	ins: in signed(31 downto 0);
	pw : out std_logic;
	mw : out std_logic;
	iord : out std_logic_vector(1 downto 0);
	iw : out std_logic;
	dw : out std_logic;
	memcode : out unsigned(2 downto 0);
	shiftinpslct : out std_logic_vector(1 downto 0);
	shiftcode : out std_logic_vector(1 downto 0);
	shiftamtslct : out std_logic_vector(1 downto 0);
	rsrc : out std_logic_vector(1 downto 0);
	rsrc2 : out std_logic;
	wdest : out std_logic_vector(1 downto 0);
	rw : out std_logic;
	m2r : out std_logic_vector(1 downto 0);
	sorm : out std_logic;
	aw : out std_logic;
	bw : out std_logic;
	asrc1 : out std_logic_vector(1 downto 0);
	asrc2 : out std_logic_vector(1 downto 0);
	opcode : out std_logic_vector(3 downto 0);
	fset : out std_logic;
	rew : out std_logic
);
end component;

signal nf, zf, cf, vf : std_logic;
signal fsets, rews, aws, bws, iws, rws, pws, dws, mws, rsrc2s, sorms : std_logic;
signal ins2ctrl : signed(31 downto 0):= "11100000000000110011000000000001";
signal iords, siselects, saselects, scodes, rsrcs, wdests, asrc1s, asrc2s, m2rs : std_logic_vector(1 downto 0);
signal memcodes : unsigned(2 downto 0);
signal opcodes : std_logic_vector(3 downto 0);  

begin

c : totalctrl port map(
	clk => topclk,
	reset => reset,
	nflag => nf,
	vflag => vf,
	cflag => cf,
	zflag => zf,
	ins => ins2ctrl,
	pw => pws,
	mw => mws,
	iord => iords,
	iw => iws,
	dw => dws,
	memcode => memcodes,
	shiftinpslct => siselects,
	shiftcode => scodes,
	shiftamtslct => saselects,
	rsrc => rsrcs,
	rsrc2 => rsrc2s,
	wdest => wdests,
	rw => rws,
	m2r => m2rs,
	sorm => sorms,
	aw => aws,
	bw => bws,
	asrc1 => asrc1s,
	asrc2 => asrc2s,
	opcode => opcodes,
	fset => fsets,
	rew => rews
); 

d : datapath port map (
	clk => topclk,
	reset => reset,
	resout => aluout,
	insout => ins2ctrl,
	pw => pws,  
	mw => mws,  
	iord => iords,  
	iw => iws,  
	dw => dws,  
	memcode => memcodes, 
	shiftinpslct => siselects,  
	shiftcode => scodes,  
	shiftamtslct => saselects,  
	rsrc => rsrcs,  
	rsrc2 => rsrc2s,  
	wdest => wdests,  
	rw => rws,  
	m2r => m2rs,  
	sorm => sorms,  
	aw => aws,  
	bw => bws,  
	asrc1 => asrc1s,  
	asrc2 => asrc2s,  
	opcode => opcodes,  
	fset => fsets,  
	rew => rews,  
	nflag => nf, 
	zflag => zf, 
	cflag => cf, 
	vflag => vf
);

end architecture;

--------------------------------------------------sas--------------------------------------------------

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
	shiftinpslct : in std_logic_vector(1 downto 0);
	shiftcode : in std_logic_vector(1 downto 0);
	shiftamtslct : in std_logic_vector(1 downto 0);
	rsrc : in std_logic_vector(1 downto 0);
	rsrc2 : in std_logic;
	wdest : in std_logic_vector(1 downto 0);
	rw : in std_logic;
	m2r : in std_logic_vector(1 downto 0);
	sorm : in std_logic;
	aw : in std_logic;
	bw : in std_logic;
	asrc1 : in std_logic_vector(1 downto 0);
	asrc2 : in std_logic_vector(1 downto 0);
	opcode : in std_logic_vector(3 downto 0);
	fset : in std_logic;
	insout : out signed(31 downto 0);
	resout : out signed(31 downto 0);
	nflag, zflag, cflag, vflag : out std_logic;
	rew : in std_logic
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
port(reset: in std_logic;
	wr_data: in signed(31 downto 0);
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

signal zero: signed(31 downto 0) := "00000000000000000000000000000000";
signal two: signed(1 downto 0) := "10";

signal memaddrtemp, memaddr : unsigned(31 downto 0);
signal mempro2mem : signed(31 downto 0); 
signal mem2irdr : signed(31 downto 0);
signal rdaddr1 : unsigned(3 downto 0);
signal rdaddr2 : unsigned(3 downto 0);
signal wraddr : unsigned(3 downto 0);
signal rfread1 : signed(31 downto 0);
signal rfread2 : signed(31 downto 0);
signal mempro2rf : signed(31 downto 0);
signal mul2b : signed(31 downto 0);
signal shiftinp : signed(31 downto 0);
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
signal splitnibble, dpimm : signed(31 downto 0);
signal memorres : signed(31 downto 0);

begin

insout <= ir;
resout <= res;

zflag <= aluz;
nflag <= alun;
vflag <= aluv;
cflag <= aluc;

dpimm <= zero(23 downto 0)&ir(7 downto 0);
splitnibble <= zero(23 downto 0)&ir(11 downto 8)&ir(3 downto 0);

shiftamt2 <= unsigned(ir(11 downto 8)*two);

memaddr <= unsigned(zero(31 downto 6))&memaddrtemp(5 downto 0);

with m2r select memorres <=
	res when "00",
	mempro2rf when "01", 
	pc when others;

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
	shift2b when '0',
	mul2b when others;

with shiftinpslct select shiftinp <=
	splitnibble when "10",
	dpimm when "01",
	rfread2 when others;	
	
with shiftamtslct select shiftamount <=
	"00000" when "00",
	unsigned(ir(11 downto 7)) when "01",
	shiftamt2(4 downto 0) when "10",
	unsigned(rfread1(4 downto 0)) when others;	

with iord select memaddrtemp <= 
	unsigned(zero(1 downto 0)&pc(31 downto 2)) when "00",
	unsigned(zero(1 downto 0)&res(31 downto 2)) when "01",
	unsigned(zero(1 downto 0)&a(31 downto 2)) when others;
	
with rsrc select rdaddr1 <= 
	unsigned(ir(19 downto 16)) when "00",
	unsigned(ir(15 downto 12)) when "01",
	unsigned(ir(11 downto 8)) when others;

with rsrc2 select rdaddr2 <= 
	unsigned(ir(15 downto 12)) when '1',
	unsigned(ir(3 downto 0)) when others;
	
with wdest select wraddr <=
	unsigned(ir(19 downto 16)) when "00",
	unsigned(ir(15 downto 12)) when "01",
	"1110" when others;
		
mem : memory port map (
	reset => reset,
	addr => memaddr,
	wr_data => mempro2mem,
	clk => clk,
	wr_enable => mw,
	rd_data => mem2irdr
	);
	
regf : regfile port map (
	rd_addr1 => rdaddr1,
	rd_addr2 => rdaddr2,
	wr_addr => wraddr,
	clk => clk,
	reset => reset,
	wr_enable => rw,
	rd_data1 => rfread1,
	rd_data2 => rfread2,
	wr_data => memorres
	);
	
mul : mult port map (
	op1 => rfread1,
	op2 => rfread2,
	result => mul2b
	);
	
shift: shifter port map (
	op => shiftinp,
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
	carry => aluc,
	result => alu2res,
	nflag => alun,
	zflag => aluz,
	cflag => aluc,
	vflag => aluv
	);
	
process(clk)
begin
	if (clk = '1' and clk'event and reset = '0') then
		case iw is 
			when '1' => ir <= mem2irdr;
			when others => null;
		end case;
		case dw is 
			when '1' => dr <= mem2irdr;
			when others => null;
		end case;
		case aw is 
			when '1' => a <= rfread1;
			when others => null;
		end case;
		case bw is 
			when '1' => b <= binp;
			when others => null;
		end case;
		case fset is 
			when '1' => nflag <= alun; zflag <= aluz;
						if ((opcode = "0000") or (opcode = "0001") or (opcode = "1100") or (opcode = "1110") or (opcode = "1111") or (opcode = "1101")) then
							cflag <= shiftcarry; else cflag <= aluc; end if;
						vflag <= aluv;				
			when others => null;
		end case;
		case rew is 
			when '1' => res <= alu2res;
			when others => null;
		end case;
		case pw is
			when '1' => pc <= alu2res;
			when others => null;
		end case;
	end if;
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
signal c : signed(0 downto 0):= "0";

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
constant one: signed(31 downto 0) := "00000000000000000000000000000001";
constant ones: signed(31 downto 0) := "00000000000000000000000000100100";

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
		regs(3) <= one;
		regs(2) <= one;
		regs(1) <= "01100101110010010100111011100100";
		regs(0) <= zero;
    elsif (clk'event and clk = '1') then
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

port(reset : in std_logic;
	wr_data: in signed(31 downto 0);
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

process(reset, clk, wr_enable, wr_data, addr)
begin
	if (reset = '1') then
		regs(1) <= "11100001110010100001000010111100";
		regs(2) <= "11100001100111100100000011010001";
		regs(0) <= "11100101101010100001000010111100";
		regs(3) <= "11101011000000000000000000000001";
		regs(4) <= "00000000000000000000000011111111";
		regs(57) <= "00000000111111111010101011001100";
    elsif (clk'event and clk = '1') then
        if (wr_enable = '1') then
            regs(to_integer(addr)) <= wr_data;
        end if;
    end if;
end process;

rd_data <= regs(to_integer(addr));

end architecture;

------------------------------------------------------------sas--------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity totalctrl is

port(clk : in std_logic;
    reset : in std_logic;
	nflag : in std_logic;
	vflag : in std_logic;
	cflag : in std_logic;
	zflag : in std_logic;
	ins: in signed(31 downto 0);
	pw : out std_logic;
	mw : out std_logic;
	iord : out std_logic_vector(1 downto 0);
	iw : out std_logic;
	dw : out std_logic;
	memcode : out unsigned(2 downto 0);
	shiftinpslct : out std_logic_vector(1 downto 0);
	shiftcode : out std_logic_vector(1 downto 0);
	shiftamtslct : out std_logic_vector(1 downto 0);
	rsrc : out std_logic_vector(1 downto 0);
	rsrc2 : out std_logic;
	wdest : out std_logic_vector(1 downto 0);
	rw : out std_logic;
	m2r : out std_logic_vector(1 downto 0);
	sorm : out std_logic;
	aw : out std_logic;
	bw : out std_logic;
	asrc1 : out std_logic_vector(1 downto 0);
	asrc2 : out std_logic_vector(1 downto 0);
	opcode : out std_logic_vector(3 downto 0);
	fset : out std_logic;
	rew : out std_logic
);
end entity;

architecture behav of totalctrl is

component mainctrl
port(
	------------------------------- Input signals
	ir : in signed(26 downto 0);
	p : in std_logic;      -- is the flag matching with the condition?
	state : in std_logic_vector(4 downto 0);
	insmode : in STD_LOGIC_VECTOR (1 downto 0);
	invalid : in std_logic;
	subinsmode : in STD_LOGIC_VECTOR (1 downto 0);
	pwrite : in STD_LOGIC_VECTOR (1 downto 0);
	imm : in std_logic_vector(1 downto 0);
	mode: in std_logic_vector(2 downto 0);
	opcod :in std_logic_vector(3 downto 0);
	set : in std_logic;
	------------------------------ Output signals
	pw : out std_logic;   --signal to check if we write to pc or not ie if branch or not
	mw : out std_logic;		--write to memory?
	iord : out std_logic_vector(1 downto 0);	-- instruction to be read from memory OR data to be written to/read from memory
	iw : out std_logic;		--give instruction as input to regfile? used in fetch state
	dw : out std_logic;		--give data as input to regfile? used in rdM state
	memcode : out unsigned(2 downto 0);
	shiftcode : out std_logic_vector(1 downto 0);
	shiftamtslct : out std_logic_vector(1 downto 0);
	shiftinpslct : out std_logic_vector(1 downto 0);
	rsrc : out std_logic_vector(1 downto 0);
	rsrc2 : out std_logic;
	wdest : out std_logic_vector(1 downto 0);
	rw : out std_logic; --write to regfile? happens in wrRF or M2RF states
	m2r : out std_logic_vector(1 downto 0);	--memory data is written to register file? other option- alu output	
	sorm : out std_logic;
	aw : out std_logic;		--#first few steps same	
	bw : out std_logic;		--#uptil separation into three.
	asrc1 : out std_logic_vector(1 downto 0);	--selects source for first input to alu
	asrc2 : out std_logic_vector(1 downto 0);	--selects source for second input to alu
	opcode : out std_logic_vector(3 downto 0);
	fset : out std_logic;	--set flags? only happens in arith state
	rew : out std_logic --write to end register after alu? 
);
end component;

component instn_decoder
port (  ins : in signed (27 downto 0);
		dinsmode : out STD_LOGIC_VECTOR (1 downto 0);
		dinvalid : out std_logic;
		dsubinsmode : out STD_LOGIC_VECTOR (1 downto 0);
		dpwrite : out STD_LOGIC_VECTOR (1 downto 0);
		dimm : out std_logic_vector(1 downto 0);
		dmode: out std_logic_vector(2 downto 0);
		dopcod: out std_logic_vector(3 downto 0);
		dset: out std_logic
);
end component;

component bctrl
port(   cond : in std_logic_vector(3 downto 0);
		nflag : in std_logic;
		zflag : in std_logic;
		cflag : in std_logic;
		vflag : in std_logic;
		p : out std_logic
);
end component;

component fsm 
port(
	clk : in std_logic;
	reset : in std_logic;
	insmode : in STD_LOGIC_VECTOR (1 downto 0);
	invalid : in std_logic;
	subinsmode : in STD_LOGIC_VECTOR (1 downto 0);
	mode: in std_logic_vector(2 downto 0);
	pwrite : in STD_LOGIC_VECTOR (1 downto 0);
	output : out std_logic_vector(4 downto 0)
);
end component;

signal p2main : std_logic;
signal dcd2imode : std_logic_vector(1 downto 0);
signal dcd2ivalid : std_logic;
signal dcd2subin : std_logic_vector(1 downto 0);
signal dcd2mode : std_logic_vector(2 downto 0);
signal dcd2pwr : std_logic_vector(1 downto 0);
signal outp2main : std_logic_vector(4 downto 0);
signal imm2main : std_logic_vector(1 downto 0);
signal opcod2main : std_logic_vector(3 downto 0);
signal set2main : std_logic;

begin

mctrl : mainctrl port map(
	------------------------------- Input signals
	ir => ins(26 downto 0),
	p => p2main,
	state => outp2main,
	insmode => dcd2imode,
	invalid => dcd2ivalid,
	subinsmode => dcd2subin,
	pwrite => dcd2pwr,
	imm => imm2main,
	mode=> dcd2mode,
	opcod => opcod2main,
	set => set2main,
	------------------------------ Output signals
	pw => pw,    
	mw => mw,		 
	iord => iord,     	 
	iw => iw,     		 
	dw => dw,     		 
	memcode => memcode,   
	shiftcode => shiftcode,     
	shiftamtslct => shiftamtslct,     
	shiftinpslct => shiftinpslct,     
	rsrc => rsrc,     
	rsrc2 => rsrc2,     
	wdest => wdest,     
	rw => rw,     
	m2r => m2r,     	
	sorm => sorm,     
	aw => aw,     	
	bw => bw,     	
	asrc1 => asrc1,  
	asrc2 => asrc2,     	 
	opcode => opcode,     
	fset => fset,     	 
	rew => rew       
);

b : bctrl port map(
	cond => std_logic_vector(ins(31 downto 28)),
	nflag => nflag,
	zflag => zflag,
	cflag => cflag,
	vflag => vflag,
	p => p2main
	);
	
fin : fsm port map(
	clk => clk,
	reset => reset,
	insmode => dcd2imode,
	invalid => dcd2ivalid,
	subinsmode => dcd2subin,
	mode => dcd2mode,
	pwrite => dcd2pwr,
	output => outp2main
	); 

dcdr : instn_decoder port map (  
	ins =>  ins(27 downto 0),
	dinsmode =>  dcd2imode,
	dinvalid =>  dcd2ivalid,
	dsubinsmode =>  dcd2subin,
	dpwrite =>  dcd2pwr,
	dimm =>  imm2main,
	dmode =>  dcd2mode,
	dopcod =>  opcod2main,
	dset =>  set2main
);
	
	
end architecture;

--------------------------------------------------sas---------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity bctrl is

port(   cond : in std_logic_vector(3 downto 0);
		nflag : in std_logic;
		zflag : in std_logic;
		cflag : in std_logic;
		vflag : in std_logic;
		p : out std_logic
);

end entity;

architecture behav of bctrl is
begin

process(cond, nflag, zflag, cflag, vflag)
begin
	case cond is
	when "0000" => if zflag = '1' then p <= '1'; else p <= '0'; end if;
	when "0001" => if zflag = '0' then p <= '1'; else p <= '0'; end if;
	when "0010" => if cflag = '1' then p <= '1'; else p <= '0'; end if;
	when "0011" => if cflag = '0' then p <= '1'; else p <= '0'; end if;
	when "0100" => if nflag = '1' then p <= '1'; else p <= '0'; end if;
	when "0101" => if zflag = '0' then p <= '1'; else p <= '0'; end if;
	when "0110" => if vflag = '1' then p <= '1'; else p <= '0'; end if;
	when "0111" => if vflag = '0' then p <= '1'; else p <= '0'; end if;
	when "1000" => if ((cflag = '1') and (zflag = '0')) then p <= '1'; else p <= '0'; end if;
	when "1001" => if ((cflag = '0') or (zflag = '1')) then p <= '1'; else p <= '0'; end if;
	when "1010" => if (nflag = zflag) then p <= '1'; else p <= '0'; end if;
	when "1011" => if (nflag = not zflag) then p <= '1'; else p <= '0'; end if;
	when "1100" => if ((nflag = zflag) and (zflag = '0')) then p <= '1'; else p <= '0'; end if;
	when "1101" => if ((nflag = not zflag) or (zflag = '1')) then p <= '1'; else p <= '0'; end if;
	when "1110" => p <= '1';
	when others => null;
	end case;
end process;

end architecture;

----------------------------------------------------sas-----------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity fsm is

port(
	--inscd : in std_logic_vector(2 downto 0);
	clk : in std_logic;
	reset : in std_logic;
   insmode : in STD_LOGIC_VECTOR (1 downto 0);
   invalid : in std_logic;
   subinsmode : in STD_LOGIC_VECTOR (1 downto 0);
   mode: in std_logic_vector(2 downto 0);
	pwrite : in STD_LOGIC_VECTOR (1 downto 0);
	output : out std_logic_vector(4 downto 0)
);

end entity; 

architecture behav of fsm is

type statetype is (fetchinit,fetch,Rab,Ra,dpAlu,wrRF,dtRab,Mrdalu,Rb,Memwb,Memwbr,Mrdres,Mrdreswr,rwbM,rwPC,Pcincr,Pc4,illegal,nimpl);
signal state : statetype := fetchinit;

begin 
	process(clk)
	begin
		if (reset = '1') then state <= fetchinit;
		elsif(clk'event and clk = '1') then
			case state is
				when fetchinit => state <= fetch;
				when fetch => 	if invalid = '1' then state <= nimpl;
								elsif (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= Rab;
								elsif (insmode(1 downto 0) = "10") then state <= rwPC;
								else state <= dtRab;
								end if;
				when Rab => 	if (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= Ra;
								else state <= illegal;
								end if;
				when Ra =>		if(insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= dpAlu;
								else state <= illegal;
								end if;
				when dpAlu =>	if (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= wrRF;
								else state <= illegal;
								end if;
				when wrRF => 	if (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= fetchinit;
								elsif (insmode(1 downto 0) = "11") then state <= rwbM;
								else state <= illegal;
								end if;		
				when dtRab => 	if (insmode(1 downto 0) = "11") then state <= Mrdalu;
								else state <= illegal;
								end if;
				when Mrdalu => 	if (insmode(1 downto 0) = "11") then state <= Rb;
								else state <= illegal;
								end if;
				when Rb =>		if ((insmode(1 downto 0) = "11") and (subinsmode(1 downto 0) = "00") and pwrite(1 downto 0)= "10") then state <= Memwb; 
								elsif ((insmode(1 downto 0) = "11") and (subinsmode(1 downto 0) = "00") and ((pwrite(1 downto 0)= "11" ) or (pwrite(1 downto 0) = "01") or (pwrite(1 downto 0) = "00"))) then state <=Memwbr;
								elsif ((insmode(1 downto 0) = "11") and (subinsmode(1 downto 0) = "01") and (pwrite(1 downto 0)= "10" )) then state <= Mrdres;
								elsif ((insmode(1 downto 0) = "11") and (subinsmode(1 downto 0) = "01") and (pwrite(1 downto 0)= "11" )) then state <= Mrdreswr;
								elsif ((insmode(1 downto 0) = "11") and (subinsmode(1 downto 0) = "01") and ((pwrite(1 downto 0) = "01") or (pwrite(1 downto 0) = "01"))) then state <= wrRF;
								else state <=  illegal;
								end if;
				when Memwb =>	if ( insmode(1 downto 0) = "11" ) then state <= fetchinit;
								else state <= illegal;
								end if;
				when Memwbr =>	if ( insmode(1 downto 0) = "11" ) then state <= fetchinit;
								else state <= illegal;
								end if;
				when Mrdres =>	if ( insmode(1 downto 0) = "11" ) then state <= rwbM;
								else state <= illegal;
								end if;
				when Mrdreswr=> if ( insmode(1 downto 0) = "11" ) then state <= rwbM;
								else state <= illegal;
								end if;
				when rwbM =>	if ( insmode(1 downto 0) = "11" ) then state <= fetchinit;
								else state <= illegal;
								end if;
				when rwPC =>	if (insmode(1 downto 0) = "10" ) then state <= Pcincr;
								else state <= illegal;
								end if;
				when Pcincr => 	if (insmode(1 downto 0) = "10" ) then state <= Pc4;
								else state <= illegal;
								end if;
				when Pc4 =>		if (insmode(1 downto 0) = "10" ) then state <= fetchinit;
								else state <= illegal;
								end if;
				when others => null;

			end case;
		end if;
	end process;
							  	
	with state select output <= 
		"00000" when fetchinit,  --
		"00001" when Rab, --shift
		"00010" when Ra,  --
 		"00011" when dpAlu, --
		"00100" when wrRF, --
		"00101" when dtRab,  --shift 
		"00110" when Mrdalu, --
		"00111" when Rb,  --
		"01000" when Memwb,   --
		"01001" when Memwbr,   --
		"01010" when Mrdres,    --
		"01011" when Mrdreswr,   --
		"01100" when rwbM,   --
		"01101" when rwPC,   --
		"01110" when Pcincr,  --
		"01111" when Pc4,  --
		"10001" when nimpl, --
		"10010" when illegal,  --
		"10011" when others;

end architecture;

-----------------------------------sas--------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mainctrl is

port(
	------------------------------- Input signals
	--instrsnip : in std_logic_vector(7 downto 0);
	   ir : in signed(26 downto 0);
	   p : in std_logic;      -- is the flag matching with the condition?
	   state : in std_logic_vector(4 downto 0);
       insmode : in STD_LOGIC_VECTOR (1 downto 0);
       invalid : in std_logic;
       subinsmode : in STD_LOGIC_VECTOR (1 downto 0);
       pwrite : in STD_LOGIC_VECTOR (1 downto 0);
       imm : in std_logic_vector(1 downto 0);
       mode: in std_logic_vector(2 downto 0);
		--u : in std_logic;
		opcod :in std_logic_vector(3 downto 0);
		set : in std_logic;
	------------------------------ Output signals
	
	pw : out std_logic;   --signal to check if we write to pc or not ie if branch or not
	--mr : out std_logic;		--read from memory?
	mw : out std_logic;		--write to memory?
	iord : out std_logic_vector(1 downto 0);	-- instruction to be read from memory OR data to be written to/read from memory
	iw : out std_logic;		--give instruction as input to regfile? used in fetch state
	dw : out std_logic;		--give data as input to regfile? used in rdM state
	
	memcode : out unsigned(2 downto 0);
	shiftcode : out std_logic_vector(1 downto 0);
	shiftamtslct : out std_logic_vector(1 downto 0);
	shiftinpslct : out std_logic_vector(1 downto 0);
	rsrc : out std_logic_vector(1 downto 0);
	rsrc2 : out std_logic;
	wdest : out std_logic_vector(1 downto 0);

	rw : out std_logic; --write to regfile? happens in wrRF or M2RF states
	m2r : out std_logic_vector(1 downto 0);	--memory data is written to register file? other option- alu output	
	sorm : out std_logic;
	aw : out std_logic;		--#first few steps same	
	bw : out std_logic;		--#uptil separation into three.

	asrc1 : out std_logic_vector(1 downto 0);	--selects source for first input to alu
	asrc2 : out std_logic_vector(1 downto 0);	--selects source for second input to alu

	opcode : out std_logic_vector(3 downto 0);
	fset : out std_logic;	--set flags? only happens in arith state
	rew : out std_logic --write to end register after alu? 
	
);

end entity;

architecture behav of mainctrl is
begin

process(pwrite,opcod, state, insmode,p)
begin
	if state = "00000" then pw <= '1'; elsif state = "01111" then pw <= p; elsif state = "01110" then pw <=p ; else pw <= '0'; end if;
	if state = "01000" then mw <= '1'; elsif state = "01001" then mw <= '1'; else mw <= '0'; end if;
	if state = "00000" then iord <= "00"; 
		elsif ((state = "00110") or (state = "01001" and (pwrite = "00" or pwrite = "01"))) then iord <= "10"; 
		elsif ((state = "01000") or (state = "01010") or (state = "01011") or (state = "01001")) then iord <= "01"; 
		else iord <= "00"; end if; ----------NOT SURE IF CORRECT 
	if state = "00000" then iw <= '1'; else iw <= '0';end if;
	if ((state = "00110") or (state = "01010") or (state = "01011")) then dw <= '1'; else dw <= '0'; end if;
	if ((state = "00011") and (insmode = "00")) then opcode <= opcod; -----------ADD code signal here
		elsif ((state = "00011") and insmode = "01") then opcode <=  "0000" ; 
		elsif ((state = "00110") and opcod(2) = '1') then opcode <= "0100";
		elsif ((state = "00110") and opcod(2) = '0') then opcode <= "0010";
		elsif (state = "00000" or state = "01101" or state = "01110" or state = "01111") then opcode <= "0100";
		else opcode <= "0000";
		end if;
	if (state = "00001") then rsrc <= "10";
		elsif ((state = "00010") and insmode = "01") then rsrc <= "01";
		elsif ((state = "00010") or (state = "00101")) then rsrc <= "00";
		else rsrc <= "00";
		end if;
	if (state = "00111") then rsrc2 <= '1'; else rsrc2 <=  '0'; end if; 
	if ((state = "00101") and (ir(22) = '1')) then shiftinpslct <= "10";    ----DANGER DANGER
		elsif (state = "00011" and ir(25) = '1') then shiftinpslct <= "01";
		else shiftinpslct <= "00"; 
	end if;
	if (state = "00011" and ir(25) = '1') then shiftamtslct <= "10";
		elsif ((state = "00011" or state = "00101") and ir(25) = '0' and ir(4) = '0') then shiftamtslct <= "01";
		elsif ((state = "00011") and ir(25) = '0' and ir(4) = '1') then shiftamtslct <= "11";
		else shiftamtslct <= "00";
	end if;
	if (state = "00011" and ir(25) = '1') then shiftcode <= "11";
		elsif ((state = "00011" or state = "00101") and ir(25) = '0') then shiftcode <= std_logic_vector(ir(6 downto 5));
	end if;
	if ((state = "00100") and ((insmode = "01") or (insmode = "11")))then wdest <= "00";
		elsif ((state = "00100") and (insmode = "00")) then wdest <= "01";
		elsif ((state = "01001") or (state = "01011")) then wdest <= "00";
		elsif ((state = "01100")) then wdest <= "01";
		elsif (state = "01100" or state = "01101") then wdest <= "10";
		else wdest <= "00";
		end if;
	if ((state = "00100") or (state = "01001") or (state = "01011") or (state = "01100") or ((state = "01101") and (subinsmode = "01"))) then rw <= '1';
		else rw <= '0';
		end if;
	if ((state = "00100" and (ir(26) = '1' or (ir(7) = '1' and ir(4) = '1'))) or (state = "01011") or (state = "01100")) then m2r <= "01" ;
		elsif (state = "01101") then m2r <= "10";
		else m2r <= "00";
		end if;
	if (insmode = "01") then sorm <= '1';
		else sorm <= '0';
		end if;
	if((state = "00001") or (state = "00101") or (state = "00111") or (state = "00010") )then bw <= '1';
		else bw <= '0';
		end if;
	if((state = "00010") or (state = "00101")) then aw <= '1';
		else aw <= '0';
		end if;
	if((state = "01110") or (state = "01111") or (state = "00000") or (state = "01101")) then asrc1 <= "00";
		elsif ((state = "00011") and (subinsmode = "00") and (insmode = "01")) then asrc1 <= "10";
		else asrc1 <= "01";
		end if;
	if (state = "01110") then asrc2 <= "11";
		elsif (state = "00110" and ir(26 downto 25) = "10") then asrc2 <= "10";
		elsif (state = "01111" or state = "00000" or state = "01101") then asrc2 <= "01";
		else asrc2 <= "00";
	end if;
	if ((state = "00011") and (set = '1') and ((insmode = "01") or (insmode = "00"))) then fset <= '1';
		else fset <= '0';
	end if;
	if ((state = "00011") or (state = "00110") ) then rew <= '1';
		else rew <= '0';
	end if;
	if (mode = "000" and ir(20) = '1') then memcode <= "000";
		elsif (mode = "000" and ir(20) = '0') then memcode <= "001";
		elsif (mode = "110" and ir(20) = '1') then memcode <= "010";
		elsif (mode = "110" and ir(20) = '0') then memcode <= "011";
		elsif (mode = "100" and ir(20) = '1') then memcode <= "100";
		elsif (mode = "100" and ir(20) = '0') then memcode <= "101";
		elsif (mode = "111" and ir(5) = '0') then memcode <= "110";
		else memcode <= "111";
	end if;
		
		 	 
end process;

end architecture; 

-----------------------------------sas-----------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity instn_decoder is
port ( ins: in signed (27 downto 0);
	   dinsmode : out STD_LOGIC_VECTOR (1 downto 0);
	   dinvalid : out std_logic;
	   dsubinsmode : out STD_LOGIC_VECTOR (1 downto 0);
	   dpwrite : out STD_LOGIC_VECTOR (1 downto 0);
	   dimm : out std_logic_vector(1 downto 0);
	   dmode: out std_logic_vector(2 downto 0);
		dopcod: out std_logic_vector(3 downto 0);
		dset: out std_logic
);
end instn_decoder;

architecture Behavioral of instn_decoder is
signal temp_insmode : std_logic_vector(2 downto 0) := "000";
signal undefined_insmode : std_logic := '0';
signal temp_subinsmode : std_logic_vector(1 downto 0):="00";
signal opcode : std_logic_vector(3 downto 0);
signal temp_mode : std_logic_vector(2 downto 0) := "000";
signal temp_pwrite : STD_LOGIC_VECTOR(1 downto 0) := "00";
signal temp_ins : signed(27 downto 0) := "0000100000110011000000000001";

begin

temp_ins <= ins;
opcode <= std_logic_vector(temp_ins(24 downto 21));
dset <= temp_ins(20);
dopcod <= std_logic_vector(temp_ins(24 downto 21));
temp_insmode <= "000" when(temp_ins(27 downto 26)="00" and (temp_ins(25)='1' or (temp_ins(25)='0' and (temp_ins(4)='0' or (temp_ins(4)='1' and temp_ins(7)='0' and not temp_ins(11 downto 8)="1111"))))) else   ----DP intr----
				"001" when(temp_ins(27 downto 26)="00" and temp_ins(25 downto 23)="000") else    --mul and mula--
              "011" when((temp_ins(27 downto 26)="00" and temp_ins(25)='0' and 
				temp_ins(4)='1' and temp_ins(7)='1' and(not (temp_ins(6 downto 5)="00"))) 
				or (temp_ins(27 downto 25)="010") 
				or (temp_ins(27 downto 25)="011" and temp_ins(4)='0')) else   ---DT inst--
              "010" when(temp_ins(27 downto 25)="101") else ---B inst----
              "100";   ----invalid---

temp_subinsmode <= "00" when (temp_insmode="010" and temp_ins(24)='0') else    ---b---
                 "01" when (temp_insmode="010" and temp_ins(24)='1') else    ---bl---
                 "00" when (temp_insmode="001" and temp_ins(21)='0' ) else   ---mul---
                 "01" when (temp_insmode="001" and temp_ins(21)='0') else    ---mla---
                 "00" when (temp_insmode="000" and temp_ins(24)='0' and (not opcode="0000") and (not opcode="0001")) else ---arith---
                 "01" when (temp_insmode="000" and (temp_ins(24)='0' or opcode(3 downto 2) = "11")) else ---logic---
                 "10" when (temp_insmode="000" and opcode(3 downto 1)="101") else ---cmp and cmn---
                 "11" when (temp_insmode="000" and opcode(3 downto 1)="100")else ---tst and teq---
                 "01" when (temp_insmode="011" and temp_ins(20)='1') else  ---ldr---
                 "00" when (temp_insmode="011" and temp_ins(20)='0');     ---str---
                 
temp_mode <= "000" when (temp_insmode="000" and temp_subinsmode="00" and opcode(2 downto 0)="010") else  ---sub---
        "001" when (temp_insmode="000" and temp_subinsmode="00" and opcode(2 downto 0)="011") else  ---rsb---
        "010" when (temp_insmode="000" and temp_subinsmode="00" and opcode(2 downto 0)="100") else  ---add---
        "011" when (temp_insmode="000" and temp_subinsmode="00" and opcode(2 downto 0)="101") else  ---adc---
        "100" when (temp_insmode="000" and temp_subinsmode="00" and opcode(2 downto 0)="110") else  ---sbc---
        "101" when (temp_insmode="000" and temp_subinsmode="00" and opcode(2 downto 0)="111") else  ---rsc---
        
        "000" when (temp_insmode="000" and temp_subinsmode="01" and opcode="0000") else  ---and---
        "001" when (temp_insmode="000" and temp_subinsmode="01" and opcode="0001") else  ---eor---
        "010" when (temp_insmode="000" and temp_subinsmode="01" and opcode="1100") else  ---orr---
        "011" when (temp_insmode="000" and temp_subinsmode="01" and opcode="1110") else  ---bic---
        "100" when (temp_insmode="000" and temp_subinsmode="01" and opcode="1101") else  ---mov---
        "101" when (temp_insmode="000" and temp_subinsmode="01" and opcode="1111") else  ---mvn---
        
        "000" when (temp_insmode="000" and temp_subinsmode="10" and opcode(0)='0') else  ---cmp---
        "001" when (temp_insmode="000" and temp_subinsmode="10" and opcode(0)='1') else  ---cmn---
        
        "000" when (temp_insmode="000" and temp_subinsmode="11" and opcode(0)='0') else  ---mov---
        "001" when (temp_insmode="000" and temp_subinsmode="11" and opcode(0)='1') else  ---mvn---
        
        "000" when (temp_ins(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="01" and opcode(1)='0') else  ---ldr---
        "110" when (temp_ins(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="01" and opcode(1)='1') else  ---ldrb---
        "100" when (temp_ins(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01" and temp_ins(6 downto 5)="01") else  ---ldrh---
        "111" when (temp_ins(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01" and temp_ins(6 downto 5)="10") else  ---ldrsb--
        "111" when (temp_ins(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01" and temp_ins(6 downto 5)="11") else  ---ldrsh--
        "100" when (temp_ins(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01") else  ---ldrh---
        "000" when (temp_ins(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="00" and opcode(1)='0') else  ---str---
        "110" when (temp_ins(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="00" and opcode(1)='1') else  ---strb---
        "100" when (temp_ins(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="00" and temp_ins(6 downto 5)="01");  ---strh---
 
temp_pwrite <= "11" when (temp_insmode(2 downto 0) = "011" and temp_ins(24)='1' and temp_ins(21)='1') else
		  "10" when (temp_insmode(2 downto 0) = "011" and temp_ins(24)='1' and temp_ins(21)='0') else
		  "01" when (temp_insmode(2 downto 0) = "011" and temp_ins(24)='0' and temp_ins(21)='1') else
		  "00" when (temp_insmode(2 downto 0) = "011" and temp_ins(24)='0' and temp_ins(21)='0');


dinsmode <= temp_insmode(1 downto 0);
dsubinsmode <= temp_subinsmode;
dmode <= temp_mode;
dinvalid <= '0';
dpwrite <= temp_pwrite;
   
dimm <=  "10" when (temp_insmode = "000" and temp_ins(25) = '1') else ------------------imm8
        "10" when (temp_insmode = "011" and temp_ins(27 downto 25) = "010") else  -----imm12 DT
        "10" when (temp_insmode = "011" and temp_ins(27 downto 25) = "000" and temp_ins(22)='1') else  ---- imm4 imm4 half word DT im offset
        "01" when (temp_insmode = "000" and temp_ins(25) = '0' and temp_ins(4) = '0') else
        "01" when (temp_insmode = "011" and temp_ins(27 downto 25) = "011") else
        "00";
                             
end Behavioral;