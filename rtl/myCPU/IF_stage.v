`include "mycpu.h"

module if_stage(
    input                          clk            ,
    input                          reset          ,
    //allwoin
    input                          ds_allowin     ,     //译码阶段允许进入
    output                         fs_allowin     ,
    //from pfs
    output                              pfs_to_fs_valid ,
    output  [`PFS_TO_FS_BUS_WD -1:0]    pfs_to_fs_bus   ,
           
    //to ds
    output                         fs_to_ds_valid ,     // 发射--->译码阶段总线有效
    output [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus   ,     //发射--->译码阶段总线  pc+inst
    
    // input                           ds_is_branch ,

    //to pfs
    output          fs_valid_o ,
    output          fs_inst_unable,

    // inst_ram interface
    input   [31:0]  inst_sram_rdata,
    input           inst_sram_data_ok,
    output          fs_inst_waiting, 

    //exception
    input   [31:0]              cp0_epc,
    input                       ws_eret,
    input                       ws_ex   
);

reg         fs_valid;       //指令发射阶段有效
wire        fs_ready_go;        //发射阶段完成处理准备
wire        fs_allowin;         //发射阶段允许进入

reg     [`PFS_TO_FS_BUS_WD -1:0]    pfs_to_fs_bus_r     ;

wire            pfs_to_fs_inst_ok ;
wire    [31:0]  pfs_to_fs_inst    ;
wire    [31:0]  fs_pc             ;

assign  {
    pfs_to_fs_inst_ok   ,
    pfs_to_fs_inst      ,
    fs_pc
}   =   pfs_to_fs_bus_r ;


//ram
reg                fs_inst_buff_valid;
reg     [31:0]     fs_inst_buff;
wire               fs_inst_ok;
wire    [31:0]     fs_inst;


wire        fs_ex ;
wire  [31:0]    fs_badvaddr ;
assign fs_to_ds_bus = {
                        fs_ex   ,       //96:96
                        fs_badvaddr ,   //95:64
                        fs_inst ,        //63:32
                        fs_pc            //31:0
                       };   //传递到译码阶段指令总线


// IF stage
assign fs_ready_go    = fs_inst_ok;   //取值阶段准备完毕
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;     //允许预取值阶段指令进入取值阶段（fs阶段空或fs准备好，且下一级流水阶段即译码允许fs进入）
assign fs_to_ds_valid =  fs_valid && fs_ready_go && !ws_ex && !ws_eret ;   //fs到ds阶段有效

always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if(ws_ex || ws_eret) begin
        fs_valid <= 1'b0 ;
    end
    else if (fs_allowin) begin
        fs_valid <= pfs_to_fs_valid;
    end

    if (pfs_to_fs_valid && fs_allowin) begin   
        pfs_to_fs_bus_r <= pfs_to_fs_bus;
    end
end

//ram

always @(posedge clk ) begin
    if(reset) begin
        fs_inst_buff_valid  <=  1'b0 ;
        fs_inst_buff        <=  32'h0 ;
    end
    else if(!fs_inst_buff_valid && fs_valid && inst_sram_data_ok && !ds_allowin) begin
        fs_inst_buff_valid  <=  1'b1 ;
        fs_inst_buff        <=  inst_sram_rdata ;        
    end
    else if(ds_allowin || ws_eret || ws_ex) begin
        fs_inst_buff_valid  <=  1'b0 ;
        fs_inst_buff        <=  32'h0 ;        
    end
end

assign  fs_inst_ok  =   pfs_to_fs_inst_ok || fs_inst_buff_valid || (fs_valid && inst_sram_data_ok) ;
assign  fs_inst     =   
            pfs_to_fs_inst_ok   ?   pfs_to_fs_inst  :
            fs_inst_buff_valid  ?   fs_inst_buff    :
            inst_sram_rdata ;

assign  fs_valid_o  =   fs_valid ;

assign fs_inst_waiting  =   fs_valid && !fs_inst_ok ;
assign fs_inst_unable   =   !fs_valid || fs_inst_buff_valid || pfs_to_fs_inst_ok ;

wire  addr_error ;
assign addr_error = (fs_pc[1:0] != 2'b00) ; 
assign fs_ex = addr_error && fs_valid ;   
assign fs_badvaddr = fs_pc ;    

endmodule
