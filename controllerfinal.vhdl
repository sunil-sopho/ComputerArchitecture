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
   insmode : in STD_LOGIC_VECTOR (1 downto 0);
   invalid : in std_logic;
   subinsmode : in STD_LOGIC_VECTOR (1 downto 0);
   imm : out std_logic_vector(1 downto 0);
   mode: in std_logic_vector(2 downto 0));
	pwrite : in STD_LOGIC_VECTOR (1 downto 0);
	output : out std_logic_vector(4 downto 0);
);

end entity; 

architecture behav of fsm is

--type statetyp is (fetch, rdAB, arith, wrRF, addr, wrM, rdM, M2RF, brn, illegal, nimpl);
--signal state : statetyp;

--begin

--process(clk)
--begin
--	if (clk'event and clk = '1') then
--		case state is
--			when fetch =>   if inscd(2 downto 1) = "11" then state <= nimpl; else state <= rdAB; end if;
--			when rdAB =>    if inscd(2 downto 1) = "00" then state <= arith; 
--							elsif inscd(2 downto 1) = "01" then state <= addr;
--							elsif inscd(2 downto 1) = "10" then state <= brn;
--							else state <= illegal; end if;
--			when arith =>	if inscd(2 downto 1) = "00" then state <= wrRF;
--							else state <= illegal; end if;
--			when wrRF =>	if inscd(2 downto 1) = "00" then state <= fetch;
--							else state <= illegal; end if;
--			when addr =>	if inscd(2 downto 0) = "010" then state <= wrM;
--							elsif inscd(2 downto 0) = "011" then state <= rdM;
--							else state <= illegal; end if;
--			when wrM =>		if inscd(2 downto 0) = "010" then state <= fetch;
--							else state <= illegal; end if;
--			when rdM =>		if inscd(2 downto 0) = "011" then state <= M2RF;
--							else state <= illegal; end if;
--			when M2RF =>	if inscd(2 downto 0) = "011" then state <= fetch;
--							else state <= illegal; end if;
--			when brn =>		if inscd(2 downto 1) = "10" then state <= fetch;
--							else state <= illegal; end if;
--			when others => null;
--		end case;
--	end if;
--end process;

--with state select output <=
--	"0000" when fetch,
--	"0001" when rdAB,
--	"0010" when arith,
--	"0011" when wrRF,
--	"0100" when addr,
--	"0101" when wrM,
--	"0110" when rdM,
--	"0111" when M2RF,
--	"1000" when brn,
--	"1001" when illegal,
--	"1010" when others;

type statetype is (fetch,Rab,Ra,dpAlu,wrRF,dtRab,Mrdalu,Rb,Memwb,Memwbr,Mrdres,Mrdreswr,rwbM,rwPC,Pcincr,Pc4,illegal,nimpl);
signal state : statetype;

begin 
	process(clk)
	begin
		if(clk'event and clk = '1') then
			case state is
				when fetch => 	if invalid = "1" then state <= nimpl;
								elsif (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= Rab;
								elsif (insmode(1 downto 0) = "10") then state <= rwPC;
								else state <= dtRab;
				when Rab => 	if (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= Ra;
								else state <= illegal;
								end if;
				when Ra =>		if(insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= dpAlu;
								else state <= illegal;
								end if;
				when dpAlu =>	if (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= wrRF;
								else state <= illegal;
								end if;
				when wrRF => 	if (insmode(1 downto 0) = "00" or insmode(1 downto 0) = "01") then state <= fetch;
								if (insmode(1 downto 0) = "11") then state <= rwbM;
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
				when Memwb =>	if ( insmode(1 downto 0) = "11" ) then state <= fetch;
								else state <= illegal;
								end if;
				when Memwbr =>	if ( insmode(1 downto 0) = "11" ) then state <= fetch;
								else state <= illegal;
								end if;
				when Mrdres =>	if ( insmode(1 downto 0) = "11" ) then state <= rwbM;
								else state <= illegal;
								end if;
				when Mrdreswr=> if ( insmode(1 downto 0) = "11" ) then state <= rwbM;
								else state <= illegal;
								end if;
				when rwbM =>	if ( insmode(1 downto 0) = "11" ) then state <= rwbM;
								else state <= illegal;
								end if;
				when rwPC =>	if (insmode(1 downto 0) = "10" ) then state <= Pcincr;
								else state <= illegal;
								end if;
				when Pcincr => 	if (insmode(1 downto 0) = "10" ) then state <= Pc4;
								else state <= illegal;
								end if;
				when Pc4 =>		if (insmode(1 downto 0) = "10" ) then state <= fetch;
								else state <= illegal;
								end if;
				when others => null;

			end case;
		end if;
	end process;
							  	
	with state select output <= 
		"00000" when fetch,
		"00001" when Rab,
		"00010" when Ra,
		"00011" when dpAlu,
		"00100" when wrRF,
		"00101" when dtRab,
		"00110" when Mrdalu,
		"00111" when Rb,
		"01000" when Memwb,
		"01001" when Memwbr,
		"01010" when Mrdres,
		"01011" when Mrdreswr,
		"01100" when rwbM,
		"01101" when rwPC,
		"01110" when Pcincr,
		"01111" when Pc4,
		"10001" when nimpl,
		"10010" when illegal,
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
	   p : in std_logic;      -- is the flag matching with the condition?
	   state : in std_logic_vector(3 downto 0);
       insmode : in STD_LOGIC_VECTOR (1 downto 0);
       invalid : in std_logic;
       subinsmode : in STD_LOGIC_VECTOR (1 downto 0);
       pwrite : in STD_LOGIC_VECTOR (1 downto 0);
       imm : in std_logic_vector(1 downto 0);
       mode: in std_logic_vector(2 downto 0));
		--u : in std_logic;
		opcode :in std_logic_vector(3 downto 0);
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
	rsrc : out std_logic_vector(1 downto 0);
	wdest : out std_logic_vector(1 downto 0);

	rw : out std_logic --write to regfile? happens in wrRF or M2RF states
	m2r : out std_logic;	--memory data is written to register file? other option- alu output	
	sorm : out std_logic;
	aw : out std_logic;		--#first few steps same	
	bw : out std_logic;		--#uptil separation into three.

	asrc1 : out std_logic(1 downto 0);	--selects source for first input to alu
	asrc2 : out std_logic_vector(1 downto 0);	--selects source for second input to alu

	opcode : out std_logic_vector(3 downto 0);
	fset : out std_logic;	--set flags? only happens in arith state
	rew : out std_logic; --write to end register after alu? 

);

end entity;

--architecture behav of mainctrl is
--begin

--process(instrsnip, state, p)
--begin
--	if state = "0000" then pw <= 1; elsif state = "1000" then pw <= p; else pw <= '0'; end if;
--	if state = "0000" then iw <= 1; else iw <= '0'; end if;
--	if state = "0110" then dw <= 1; else dw <= '0'; end if;
--	if state = "0001" then aw <= 1; else aw <= '0'; end if;
--	if state = "0001" then bw <= 1; else bw <= '0'; end if;
--	if ((state = "0010") or (state = "0100")) then resw <= '1'; else resw <= '0'; end if;
--	if state = "0010" then fset <= p; else fset <= '0'; end if;
--	if ((state = "0011") or (state = "0111")) then rw <= p; else rw <= '0'; end if;
--	if ((state = "0000") or (state = "0110")) then mr <= '1'; else mr <= '0'; end if;
--	if state = "0101" then mw <= p; else mw <= '0'; end if;
--	if (state = "0000") then iord <= '0'; elsif ((state = "0101") or (state = "0110")) then iord <= '1'; end if;
--	if ((state = "0000") or (state = "1000")) then asrc1 <= '0'; 
--		elsif ((state = "0010") or (state = "0100")) then iord <= '1'; end if;
--	if (state = "0000") then asrc2 <= "01"; 
--		elsif state = "0010" then asrc2 <= "00";
--		elsif state = "0100" then asrc2 <= "10";
--		elsif state = "1000" then asrc2 <= "11"; end if;
--	if state = "0001" then rsrc <= "0"; elsif state = "0100" then rsrc <= "1"; end if;
--	if state = "0011" then m2r <= "0"; elsif state = "0111" then rsrc <= "1"; end if;
--end process;


--end architecture; 

architecture behav of mainctrl is
begin

process(pwrite,opcode, state, insmode,p)
begin
	--if state = "0000" then pw <= 1; elsif state = "1000" then pw <= p; else pw <= '0'; end if;
	--if state = "0000" then iw <= 1; else iw <= '0'; end if;
	--if state = "0110" then dw <= 1; else dw <= '0'; end if;
	--if state = "0001" then aw <= 1; else aw <= '0'; end if;
	--if state = "0001" then bw <= 1; else bw <= '0'; end if;
	--if ((state = "0010") or (state = "0100")) then resw <= '1'; else resw <= '0'; end if;
	--if state = "0010" then fset <= p; else fset <= '0'; end if;
	--if ((state = "0011") or (state = "0111")) then rw <= p; else rw <= '0'; end if;
	--if ((state = "0000") or (state = "0110")) then mr <= '1'; else mr <= '0'; end if;
	--if state = "0101" then mw <= p; else mw <= '0'; end if;
	--if (state = "0000") then iord <= '0'; elsif ((state = "0101") or (state = "0110")) then iord <= '1'; end if;
	--if ((state = "0000") or (state = "1000")) then asrc1 <= '0'; 
	--	elsif ((state = "0010") or (state = "0100")) then iord <= '1'; end if;
	--if (state = "0000") then asrc2 <= "01"; 
	--	elsif state = "0010" then asrc2 <= "00";
	--	elsif state = "0100" then asrc2 <= "10";
	--	elsif state = "1000" then asrc2 <= "11"; end if;
	--if state = "0001" then rsrc <= "0"; elsif state = "0100" then rsrc <= "1"; end if;
	--if state = "0011" then m2r <= "0"; elsif state = "0111" then rsrc <= "1"; end if;

	if state = "00000" then pw <= '1'; elsif state = "01111" then pw <= p; elsif state = "01110" then pw <=p ; else pw <= '0'; end if;
	if state = "01000" then mw <= '1'; elsif state = "01001" then mw <= 1; else mw <= '0'; end if;
	if state = "00000" then iord <= "00"; 
		elsif ((state = "00110") or (state = "01001" and (pwrite = "00" or pwrite = "01"))) then iord <= "01"; 
		elsif ((state = "01000") or (state = "01010") or (state = "01011")) then iord <= "10"; 
		else iord <= '00'; end if; ----------NOT SURE IF CORRECT 
	if state = "00000" then iw <= '1'; else iw <= '0';end if;
	if ((state = "00110") or (state = "01010") or (state = "01011")) then dw <= "1";else dw <= '0'; end if;
	if ((state = "00011") and (insmode = "00")) then opcode <= opcode; -----------ADD code signal here
		elsif ((state = "00011") and insmode = "01") then opcode <=  "0000" ; 
		elsif ((state = "01111") or (state = "01110") then opcode <= "0000" ;
		elsif ((state = "00110") and opcode(2)='1') then opcode <= "0000";
		elsif ((state = "00110") and opcode(2) = '0') then opcode <= "0010";
		else memcode <= "0000";
		end if;
	if (state = "00001") then rsrc <= "10";
		elsif ((state = "00010") and insmode = "01") then rsrc <= "01";
		elsif ((state = "00010") or state = "00101") then rsrc <= "00";
		else rsrc <= "00";
		end if;
	------------------------------shiftcod and shiftamfslt left also look at rb condition need a multiplexer----------------------------------------------
	if ((state = "00100") and ((insmode = "01") or (insmode = "11")))then wdest <= "00";
		elsif ((state = "00100") and (insmode = "00")) then wdest <= "01";
		elsif ((state = "01001") or (state = "01011")) then wdest <= "00";
		elsif ((state = "01100")) then wdest <= "01";
		elsif (state = "01100") then wdest <= "10";
			
		else wdest <= "00";
		end if;
	if ((state = "00100") or (state = "01001") or (state = "01011") or (state = "01100") or ((state = "01101") and (subinsmode = "01")) then rw <= '1';
		else rw <= '0';
		end if;
	if (state = "01100") then m2r <= '0';
		elsif ((state = "00100") or (state = "01001") or (state = "01011")) then m2r <= '1' ;
		else m2r <= '0';
		end if;
	if (insmode = "01") then sorm <= '1';
		else then sorm <= '0';
		end if;
	if((state = "00001") or (state = "00101") or (state = "00111") )then bw <= '1';
		else bw <= '0';
		end if;

	if((state = "00010") or (state = "00101")) aw <= '1';
		else aw <= '0';
		end if;
	if((state = "01110") or (state = "01111")) then asrc1 <= "00";
		elsif ((state = "00011") and (subinsmode = "00") and (insmode = "01")) then asrc1 <= "10";
		else asrc1 <= "01";
		end if;

--------------------------------CONFUSED NOT SURE ABOUT THIS-------------------------------------------------
	if(state = "01111") asrc2 <= "01";
		elsif (state = "01110") then asrc2 <= "11";
		--elsif (state) then
	end if;

	if ((state = "00011") and (set = '1') and ((insmode = "01") or (insmode = "00"))) then fset <= '1';
		else fset <= '0';
	end if;

	if ((state = "00011") or (state = "00110") ) rew <= '1';
		else rew <= '0';
	end if;
			
		
		 	 
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




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity instn_decoder is
    Port ( ins2 : in STD_LOGIC_VECTOR (11 downto 4);
           ins1 : in STD_LOGIC_VECTOR (27 downto 15);
           insmode : out STD_LOGIC_VECTOR (1 downto 0);
           invalid : out std_logic;
           subinsmode : out STD_LOGIC_VECTOR (1 downto 0);
           pwrite : out STD_LOGIC_VECTOR (1 downto 0);
           imm : out std_logic_vector(1 downto 0);
           mode: out std_logic_vector(2 downto 0));
    		opcode: out std_logic_vector(3 downto 0);
    		set: out std_logic;
end instn_decoder;

architecture Behavioral of instn_decoder is
signal temp_insmode : std_logic_vector(2 downto 0) := "000";
signal undefined_insmode : std_logic := '0';
signal temp_subinsmode : std_logic_vector(1 downto 0):="00";
--signal opcode : std_logic_vector(3 downto 0);
signal temp_mode : std_logic_vector(2 downto 0) := "000";
signal temp_pwrite : STD_LOGIC_VECTOR(1 downto 0) := "00";

begin
set <= ins1(20);
opcode<= ins1(24 downto 21);
temp_insmode <= "001" when(ins1(27 downto 26)="00" and ins1(25 downto 23)="000") else    --mul and mula--
              "000" when(ins1(27 downto 26)="00" and (ins1(25)='1' or (ins1(25)='0' and (ins2(4)='0' or (ins2(4)='1' and ins2(7)='0' and not ins2(11 downto 8)="1111"))))) else   ----DP intr----
              "011" when((ins1(27 downto 26)="00" and ins1(25)='0' and (ins2(4)='1' and ins2(7)='1' and(not (ins2(6 downto 5)="00")))) or (ins1(27 downto 25)="010" or (ins1(27 downto 25)="011" and ins2(4)='0'))) else   ---DT inst--
              "010" when(ins1(27 downto 25)="101") else ---B inst----
              "100";   ----invalid---

temp_subinsmode <= "00" when (temp_insmode="010" and ins1(24)='0') else    ---b---
                 "01" when (temp_insmode="010" and ins1(24)='1') else    ---bl---
                 "00" when (temp_insmode="001" and ins1(21)='0' ) else   ---mul---
                 "01" when (temp_insmode="001" and ins1(21)='0') else    ---mla---
                 "00" when (temp_insmode="000" and ins1(24)='0' and not opcode="0000" and not opcode="0001") else ---arith---
                 "01" when (temp_insmode="000" and (ins1(24)='0' or opcode(3 downto 2) = "11")) else ---logic---
                 "10" when (temp_insmode="000" and opcode(3 downto 1)="101") else ---cmp and cmn---
                 "11" when (temp_insmode="000" and opcode(3 downto 1)="100")else ---tst and teq---
                 "01" when (temp_insmode="011" and ins1(20)='1') else  ---ldr---
                 "00" when (temp_insmode="011" and ins1(20)='0');     ---str---
                 
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
        
        "000" when (ins1(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="01" and opcode(1)='0') else  ---ldr---
        "110" when (ins1(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="01" and opcode(1)='1') else  ---ldrb---
        "100" when (ins1(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01" and ins2(6 downto 5)="01") else  ---ldrh---
        "111" when (ins1(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01" and ins2(6 downto 5)="10") else  ---ldrsb--
        "111" when (ins1(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01" and ins2(6 downto 5)="11") else  ---ldrsh--
        "100" when (ins1(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="01") else  ---ldrh---
        "000" when (ins1(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="00" and opcode(1)='0') else  ---str---
        "110" when (ins1(27 downto 26)="01" and temp_insmode="011" and temp_subinsmode="00" and opcode(1)='1') else  ---strb---
        "100" when (ins1(27 downto 26)="00" and temp_insmode="011" and temp_subinsmode="00" and ins2(6 downto 5)="01");  ---strh---
 
temp_pwrite <= "11" when (temp_insmode(2 downto 0) = "011" and ins1(24)="1" and ins1(22)="1") else
		  "10" when (temp_insmode(2 downto 0) = "011" and ins1(24)="1" and ins1(22)="0") else
		  "01" when (temp_insmode(2 downto 0) = "011" and ins1(24)="0" and ins1(22)="1") else
		  "00" when (temp_insmode(2 downto 0) = "011" and ins1(24)="0" and ins1(22)="0");


insmode <= temp_insmode(1 downto 0);
subinsmode <= temp_subinsmode;
mode <= temp_mode;
invalid <= temp_insmode(2);
pwrite <= temp_pwrite;
   
imm <=  "10" when (temp_insmode = "000" and ins1(25) = '1') else ------------------imm8
        "10" when (temp_insmode = "011" and ins1(27 downto 25) = "010") else  -----imm12 DT
        "10" when (temp_insmode = "011" and ins1(27 downto 25) = "000" and ins1(22)='1') else  ---- imm4 imm4 half word DT im offset
        "01" when (temp_insmode = "000" and ins1(25) = '0' and ins2(4) = '0') else
        "01" when (temp_insmode = "011" and ins1(27 downto 25) = "011") else
        "00";
                             
end Behavioral;
