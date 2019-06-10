//Generic mux for data and instruction
module Mux_2X1(input[31:0]in1,in2,input selection,output[31:0]out);
assign out =(selection==0)?in1:(selection==1)?in2:32'bx;
endmodule
//generic mux for addresses
module Mux_2X1_5b(input wire [4:0]in1,in2,input selection,output[4:0]out);
assign out =(selection==0)?in1:(selection==1)?in2:5'bx;
endmodule
//Instruction Memory
module InstructionMemory(input[31:0]pc,output[31:0]IR);
reg [31:0] Imemory [1023:0];
assign IR =Imemory[pc>>2];
//initialization for test purpose
initial         
$readmemh("Instruction Memory.txt",Imemory);
endmodule
//Register File
module RegFile(input[4:0]ReadReg1,ReadReg2,WriteReg,input[31:0]WriteData,input write,output[31:0]readData1,readData2);
parameter zero=0,t0=8,t1=9,t2=10,t3=11,t4=12,t5=13,t6=14,t7=15,t8=24,t9=25,
s0=16,s1=17,s2=18,s3=19,s4=20,s5=21,s6=22,s7=23;
integer register [0:31];
always @(write)
begin
if((write==1)&&(WriteReg!=0))
register[WriteReg]<=WriteData;
else
register[WriteReg]<=register[WriteReg];
end
assign readData1=register[ReadReg1];
assign readData2=register[ReadReg2];
//Initialization for test purpose
initial
fork
register[zero]=0; register[t0]=1; register[t1]=2; register[t2]=3; register[t3]=-1;register[t4]=-2;register[t5]=-30;register[t6]=125;
register[t7]=-17;register[t8]=-56;register[t9]=64;
register[s0]=-5;register[s1]=7;register[s2]=3;register[s3]=6;register[s4]=99;register[s5]=74;register[s6]=74;register[s7]=71;
join
endmodule
//Main Control
module MainControl(input[5:0]opcode,output reg [12:0]control);
parameter R_type=0,LW=35,SW=43,Beq=4,Bne=5,Andi=12,Ori=13,Addi=8,Lui=15,Slti=10,j=2,jal=3; //Op codes
always @(opcode)
begin
case(opcode)            //RegDst_AluSrc_MemtoReg_RegWrite_MemRead_MemWrite_Branch_BranchNE_2Jump_3Aluop_IFFlush
	LW:     control<=13'b0______1______1________1________1_______0________0______0________00____000;
	SW:     control<=13'bx______1______x________0________0_______1________0______0________00____000;
	Addi:   control<=13'b0______1______0________1________0_______0________0______0________00____000;
	Beq:    control<=13'bx______0______x________0________0_______0________1______0________00____001;
	Bne:    control<=13'bx______0______x________0________0_______0________0______1________00____001;
	R_type: control<=13'b1______0______0________1________0_______0________0______0________00____010;
	Andi:   control<=13'b0______1______0________1________0_______0________0______0________00____011;
	Ori:    control<=13'b0______1______0________1________0_______0________0______0________00____100;
	Lui:    control<=13'b0______1______0________1________0_______0________0______0________00____101;
	Slti:   control<=13'b0______1______0________1________0_______0________0______0________00____110;
	j:      control<=13'bx______x______x________0________0_______0________0______0________01____00x;
endcase
end
endmodule 
//SignExtension
module SignExtend(input wire[15:0]immediate_16,output wire[31:0]immediate_32);
parameter MSB=15;//The sign bit
assign immediate_32= {{16{immediate_16[MSB]}},immediate_16};
endmodule 
//Alu Control
module AluControl(input wire [2:0]Aluop,input wire [5:0]funct,output wire [3:0]operation);
//Input Signals
parameter IType_lw_sw_addi=0/*add*/,IType_beq_bne=1/*sub*/,R_type=2/*check func field*/,IType_Andi=3,IType_Ori=4,IType_Lui=5,IType_Slti=6;
//Function field values
parameter func_And=36,func_Or=37,func_Add=32,func_Sub=34,func_Nor=39,func_Sll=0,func_Srl=2,func_Slt=42,func_Jr=8;
//outputus (operations drive signals)
parameter And=0,Or =1,Add=2,Sub=3,Nor=4,sll=5,srl=6,sra=7,slt=8/*,great=9*/,sll_16=10;//last for slti
assign operation=(Aluop==IType_lw_sw_addi)?Add:(Aluop==IType_beq_bne)?Sub:(Aluop==IType_Andi)?And:(Aluop==IType_Ori)?Or:
(Aluop==IType_Lui)?sll_16:(Aluop==IType_Slti)?slt:((Aluop==R_type)&&(funct==func_And))?And:((Aluop==R_type)&&(funct==func_Or))?Or:
((Aluop==R_type)&&(funct==func_Add))?Add:((Aluop==R_type)&&(funct==func_Sub))?Sub:((Aluop==R_type)&&(funct==func_Nor))?Nor:
((Aluop==R_type)&&(funct==func_Sll))?sll:((Aluop==R_type)&&(funct==func_Srl))?srl:((Aluop==R_type)&&(funct==func_Slt))?slt:4'bx;
endmodule
//ALU Module
module ALU(input1,input2,operation,sAmount,out,zero);
//Mips Alu Operation Codes
parameter And=0,Or =1,Add=2,Sub=3,Nor=4,sll=5,srl=6,sra=7,slt=8,great=9,sll_16=10;
//Mips Alu inputs
input wire[4:0] sAmount;
input signed  [31:0] input1,  input2;
input wire [3:0] operation;
output wire [31:0] out;
output wire zero;
assign out =(operation==And)?input1&input2:(operation==Or)?input1|input2:(operation==Add)?input1+input2:
(operation==Sub)?input1-input2:(operation==Nor)?~(input1|input2):(operation==sll)?(input2<<sAmount):
(operation==srl)?(input2>>sAmount):(operation==sra)?(input2>>>sAmount):
((operation==slt)&&(input1<input2))?32'b1:(operation==slt)&&(input1>=input2)?32'b0:(operation==sll_16)?(input2<<16):32'bx;
assign zero =((operation==Sub)&&(input1==input2))?1'b1:((operation==Sub)&&(input1!=input2))?1'b0:1'bx;
endmodule
//Data Memory
module DataMemory (input wire[31:0]address,write_data,input wire memwrite,memread,output wire[31:0]read_data);
reg [31:0] memory[1023:0]; 
initial
begin memory[78]<=11; end
assign read_data =(memread==1'b1)?memory[address]:32'bx;
always @(memwrite)
if (memwrite==1'b1) 
begin
memory[address] <= write_data;
end
else
memory[address] <= memory[address];
endmodule
//The processor
`timescale 1ps / 1ps
module CPU();
reg clock;
reg [31:0]PC;
wire [31:0]Ir;
wire [31:0]read1,read2;//register file output
wire [31:0]sign_out; //extended immediate 
//Instruction fields
wire [5:0]op=Ir[31:26];
wire [4:0]rs=Ir[25:21];
wire [4:0]rt=Ir[20:16];
wire [4:0]rd=Ir[15:11];
wire [4:0]shamt=Ir[10:6];
wire [5:0]func=Ir[5:0];
wire [15:0]immediate ={rd,shamt,func};
wire [31:0]WriteData;
wire [12:0] control;
wire RegDst,AluSrc,MemtoReg,RegWrite,MemRead,MemWrite,Branch,BranchNE;//main control signals
wire [1:0]Jump;//main control signal
wire [2:0]Aluop;//main control signal
assign {RegDst,AluSrc,MemtoReg,RegWrite,MemRead,MemWrite,Branch,BranchNE,Jump,Aluop}=control;
wire [3:0]AluOperation;//alu control signals
wire Pcsrc,Zeroflag;//for branch
wire [4:0]writeReg;
wire [31:0]ALUin2,ALUout,DataMem_read;
assign Pcsrc = (Branch&Zeroflag)|(BranchNE &(~Zeroflag));
InstructionMemory Imem(PC,Ir);
MainControl testc(op,control);
Mux_2X1_5b RegDest(rt,rd,RegDst,writeReg);//finished
RegFile regf(rs,rt,writeReg,WriteData,RegWrite,read1,read2); //finished
Mux_2X1 Alu_Src(read2,sign_out,AluSrc,ALUin2);//finished
SignExtend sign_ext(immediate,sign_out);//finished
AluControl alucon(Aluop,func,AluOperation);//finished
ALU al(read1,ALUin2,AluOperation,shamt,ALUout,Zeroflag);//finished double checked
DataMemory Dmem(ALUout, read2, MemWrite, MemRead, DataMem_read);//finished
Mux_2X1 memToReg(ALUout,DataMem_read,MemtoReg,WriteData);
always 
begin 
#3
 clock=~clock;
end
initial 
begin
PC<=0;
clock <=0;
$display("                      op rs rt       aluout      writedata");
$monitor($time,,,"%d %d %d %d %d",op,rs,rt,ALUout,WriteData);
end
always@(posedge clock)
begin
	if(Jump==2'b00)
	begin
		if(Pcsrc==0)
		begin
		PC<=PC+4;
		end
		else if(Pcsrc==1)
		begin
		PC<=(PC+4)+(sign_out<<2);
		end	
	end
	else if(Jump==2'b01)
	begin	
	PC<={PC[31:28],(immediate<<2)};
	end
end
endmodule 