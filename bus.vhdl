entity masterstate is
port (
	clk,reset : in std_logic;
	port0;port1;port2;port3: in std_logic;
	mem_sel : in std_logic;
)

end entity;

--0000 addr
--0001 wait1
--0010 wait2
--0011 wait3
--0100 swi
--0101 led
--0110 pos 
--0111 pat

architecture behav of masterstate is
signal state : std_logic_vector(3 downto 0);
state = "1111";
begin process(clk,reset)
	begin
		if(reset = '1')
		then	state = "0000";
		elsif(clk event and clk = '1') then
			case state is
				when "1111" => state <= "0000";
				when "0000" => 
					if (mem_sel = "1") then state <= "0001";
					elsif (port0 = "1") then state <= "0100";
					elsif (port1 = "1") then state <= "0101";
					elsif (port2 = "1") then state <= "0110";
					end if;
				when "0001" => state <= "0010";
				when "0010" => state <= "0011";
				when "0011" => state <= "0000";
				when "0100" => state <= "0000";
				when "0101" => state <= "0000";
				when "0110" => state <= "0000";
			end case;
		end if;
	end process;
end process;
end architecture;
	
	
	--------------------------------------------------------------------------
	
entity control is
port(
	clk,reset: in std_logic;
	haddr: in std_logic_vector(6 downto 0);
	hdata: in std_logic_vector(31 downto 0);
	hready: in std_logic;
	hready: in std_logic;
	wr: in std_logic;
	size: in std_logic_vector(2 downto 0);
	state: in std_logic_vector(3 downto 0);
--	trans: in std_logic_vector(1 downto 0);
	
	oaddr: out std_logic_vector(6 downto 0);
	hwrite: out std_logic;
	hsize:out std_logic_vector(2 downto 0);
	hwdata: out std_logic_vector(31 downto 0);
	htrans: out	std_logic_vector(1 downto 0);

)
end entity;	

architecture behav of control is
begin 
process(state,wr)
		if 	state="0000" then oaddr <= haddr;end if;
		if state="0000" then htrans <= "10" elsif (state="0001" or state="0010") then htrans <="00";end if;
		if state = "0000" then hwrite <= wr;end if;
		if state = "0000" then hsize <= size;end if;
		if (state = "0011" and wr = '0') then hwdata <= hdata;end if;
end process;

end architecture;


	
entity slave is 
port ( --- input signal
	hsel : in std_logic;
	haddr: in std_logic_vector(6 downto 0);
	hwrite: in std_logic;
	hsize: in std_logic;
	htrans : in std_logic_vector(1 downto 0);
	hready : in std_logic;
	hwdata: in std_logic_vector(31 downto 0);

	clk,reset: in std_logic;

	----out signal
	hreadyout : out std_logic;
	hresp : out std_logic;
	hrdata : out std_logic_vector(31 downto 0);
	     )
end entity;


































