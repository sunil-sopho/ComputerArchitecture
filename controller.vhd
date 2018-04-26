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

port(inscd : in std_logic_vector(2 downto 0);
	clk : in std_logic;
	output : out std_logic_vector(3 downto 0)
);

end entity; 

architecture behav of fsm is

type statetyp is (fetch, rdAB, arith, wrRF, addr, wrM, rdM, M2RF, brn, illegal, nimpl);
signal state : statetyp;

begin

process(clk)
begin
	if (clk'event and clk = '1') then
		case state is
			when fetch =>   if inscd(2 downto 1) = "11" then state <= nimpl; else state <= rdAB; end if;
			when rdAB =>    if inscd(2 downto 1) = "00" then state <= arith; 
							elsif inscd(2 downto 1) = "01" then state <= addr;
							elsif inscd(2 downto 1) = "10" then state <= brn;
							else state <= illegal; end if;
			when arith =>	if inscd(2 downto 1) = "00" then state <= wrRF;
							else state <= illegal; end if;
			when wrRF =>	if inscd(2 downto 1) = "00" then state <= fetch;
							else state <= illegal; end if;
			when addr =>	if inscd(2 downto 0) = "010" then state <= wrM;
							elsif inscd(2 downto 0) = "011" then state <= rdM;
							else state <= illegal; end if;
			when wrM =>		if inscd(2 downto 0) = "010" then state <= fetch;
							else state <= illegal; end if;
			when rdM =>		if inscd(2 downto 0) = "011" then state <= M2RF;
							else state <= illegal; end if;
			when M2RF =>	if inscd(2 downto 0) = "011" then state <= fetch;
							else state <= illegal; end if;
			when brn =>		if inscd(2 downto 1) = "10" then state <= fetch;
							else state <= illegal; end if;
			when others => null;
		end case;
	end if;
end process;

with state select output <=
	"0000" when fetch,
	"0001" when rdAB,
	"0010" when arith,
	"0011" when wrRF,
	"0100" when addr,
	"0101" when wrM,
	"0110" when rdM,
	"0111" when M2RF,
	"1000" when brn,
	"1001" when illegal,
	"1010" when others;

end architecture;

-----------------------------------sas--------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mainctrl is

port(instrsnip : in std_logic_vector(7 downto 0);
	p : in std_logic;      -- is the flag matching with the condition?
	state : in std_logic_vector(3 downto 0);
	iord : out std_logic;	-- instruction to be read from memory OR data to be written to/read from memory
	pw : out std_logic;   --signal to check if we write to pc or not ie if branch or not
	mr : out std_logic;		--read from memory?
	mw : out std_logic;		--write to memory?
	iw : out std_logic;		--give instruction as input to regfile? used in fetch state
	dw : out std_logic;		--give data as input to regfile? used in rdM state
	m2r : out std_logic;	--memory data is written to register file? other option- alu output	
	fset : out std_logic;	--set flags? only happens in arith state
	asrc1 : out std_logic;	--selects source for first input to alu
	asrc2 : out std_logic_vector(1 downto 0);	--selects source for second input to alu
	rsrc : out std_logic;	--#combine to ensure
	aw : out std_logic;		--#first few steps same	
	bw : out std_logic;		--#uptil separation into three.
	resw : out std_logic; --write to end register after alu? 
	rw : out std_logic --write to regfile? happens in wrRF or M2RF states
);

end entity;

architecture behav of mainctrl is
begin

process(instrsnip, state, p)
begin
	if state = "0000" then pw <= 1; elsif state = "1000" then pw <= p; else pw <= '0'; end if;
	if state = "0000" then iw <= 1; else iw <= '0'; end if;
	if state = "0110" then dw <= 1; else dw <= '0'; end if;
	if state = "0001" then aw <= 1; else aw <= '0'; end if;
	if state = "0001" then bw <= 1; else bw <= '0'; end if;
	if ((state = "0010") or (state = "0100")) then resw <= '1'; else resw <= '0'; end if;
	if state = "0010" then fset <= p; else fset <= '0'; end if;
	if ((state = "0011") or (state = "0111")) then rw <= p; else rw <= '0'; end if;
	if ((state = "0000") or (state = "0110")) then mr <= '1'; else mr <= '0'; end if;
	if state = "0101" then mw <= p; else mw <= '0'; end if;
	if (state = "0000") then iord <= '0'; elsif ((state = "0101") or (state = "0110")) then iord <= '1'; end if;
	if ((state = "0000") or (state = "1000")) then asrc1 <= '0'; 
		elsif ((state = "0010") or (state = "0100")) then iord <= '1'; end if;
	if (state = "0000") then asrc2 <= "01"; 
		elsif state = "0010" then asrc2 <= "00";
		elsif state = "0100" then asrc2 <= "10";
		elsif state = "1000" then asrc2 <= "11"; end if;
	if state = "0001" then rsrc <= "0"; elsif state = "0100" then rsrc <= "1"; end if;
	if state = "0011" then m2r <= "0"; elsif state = "0111" then rsrc <= "1"; end if;
end process;


end architecture; 

--------------------------------------------------sas-----------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity actrl is 
port(state : in std_logic_vector(3 downto 0);
	insop : in std_logic_vector(3 downto 0);
	opcode : out std_logic_vector(3 downto 0) 
);

end entity;

architecture behav of actrl is
begin

process(state, insop)
begin
	case state is
		when "0000" => opcode <= "0100";
		when "0010" => opcode <= insop;
		when "0100" => if insop(2) = '1' then opcode <= "0100"; else opcode <= "0010"; end if;
		when "1000" => opcode <= "0100";
		when others => null;
	end case;
end process;

end architecture;

-----------------------------------sas-----------------------------------------------
