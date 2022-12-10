`include "mycpu.h"

module if_stage(
    input                          clk            ,
    input                          reset          ,
    //allwoin
    input                          ds_allowin     ,     //译码阶段允许进入
    //brbus
    input  [`BR_BUS_WD       -1:0] br_bus         ,     //译码阶段总线
    //to ds
    output                         fs_to_ds_valid ,     // 发射--->译码阶段总线有效
    output [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus   ,     //发射--->译码阶段总线  pc+inst
    
    input                           ds_is_branch ,
    // inst sram interface
    output        inst_sram_en   ,     //指令ram使能
    output [ 3:0] inst_sram_wen  ,      //字节写使能
    output [31:0] inst_sram_addr ,      //写地址
    output [31:0] inst_sram_wdata,      //写数据
    input  [31:0] inst_sram_rdata,       //读数据

    //exception
    input   [31:0]              cp0_epc,
    input                       ws_eret,
    input                       ws_ex   ,
    input                       ws_ex1
);

reg         fs_valid;       //指令发射阶段有效
wire        fs_ready_go;        //发射阶段完成处理准备
wire        fs_allowin;         //发射阶段允许进入
wire        to_fs_valid;        //进入发射阶段的指令有效



wire [31:0] seq_pc;         
wire [31:0] nextpc;             //下一指令地址

wire         br_taken;          //是否为跳转指令
wire [ 31:0] br_target;         //跳转目的地址
wire            br_stall ;
assign {br_stall,br_taken,br_target} = br_bus;       //跳转总线分解

wire [31:0] fs_inst;   //取出的指令
reg  [31:0] fs_pc;      //取出的指令对应地址

wire        fs_ex ;
wire        fs_bd ;
wire  [31:0]    fs_badvaddr ;
assign fs_to_ds_bus = {
                        fs_ex   ,       //97:97
                        fs_bd   ,       //96:96
                        fs_badvaddr ,   //95:64
                       fs_inst ,        //63:32
                       fs_pc            //31:0
                       };   //传递到译码阶段指令总线

// pre-IF stage      

wire            pre_fs_ready_go ;

assign      pre_fs_ready_go = ~br_stall ;

assign to_fs_valid  = ~reset && pre_fs_ready_go;
assign seq_pc       = fs_pc + 3'h4;     //正常指令地址，+4
assign nextpc       =   ws_ex    ? 32'hbfc00380         :
                        ws_eret  ? cp0_epc      :
                        br_taken ? br_target : 
                                    seq_pc  ;   //二选一选择器，是否为跳转指令或正常指令 

// IF stage
assign fs_ready_go    = 1'b1;   //取值阶段准备完毕
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;     //允许预取值阶段指令进入取值阶段（fs阶段空或fs准备好，且下一级流水阶段即译码允许fs进入）
assign fs_to_ds_valid =  fs_valid && fs_ready_go && !ws_ex && !ws_eret ;   //fs到ds阶段有效
always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end

    if (reset) begin
        fs_pc <= 32'hbfbffffc;  //trick: to make nextpc be 0xbfc00000 during reset 
    end
    else if (to_fs_valid && fs_allowin) begin   //允许取下一条指令
        fs_pc <= nextpc;
    end
end

assign inst_sram_en    = to_fs_valid && fs_allowin ;
assign inst_sram_wen   = 4'h0;
assign inst_sram_addr  = nextpc;
// assign inst_sram_addr  = {nextpc[31:2], 2'b0};
assign inst_sram_wdata = 32'b0;     

assign fs_inst         = inst_sram_rdata;

wire  addr_error ;
assign addr_error = (fs_pc[1:0] != 2'b00) ; 
assign fs_ex = addr_error && fs_valid ; 
assign fs_bd = ds_is_branch ;   
assign fs_badvaddr = fs_pc ;    

endmodule
