`include "mycpu.h"

module mem_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          ws_allowin    ,
    output                         ms_allowin    ,
    //from es
    input                          es_to_ms_valid,
    input  [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus  ,
    //to ws
    output                         ms_to_ws_valid,
    output [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus  ,
    //from data-sram
    // input  [31                 :0] data_sram_rdata,

    //to es
    output                         ms_inst_unable,

    //from data-sram
    input  [31                 :0] data_sram_rdata,
    input                          data_sram_data_ok,
    output                         ms_data_waiting,

    output      [4:0]       MEM_dest,
    output      [31:0]      MEM_result,

    //block
    output                  ms_inst_mfc0_o,

    output                  to_ws_ex ,

    input                   ws_ex   ,
    input                   ws_eret ,
    output                  ms_ex_o ,
    output                  ms_eret 
);

reg         ms_valid;
wire        ms_ready_go;

reg [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus_r;
wire        ms_res_from_mem;
wire        ms_gr_we;
wire [ 4:0] ms_dest;
wire [31:0] ms_alu_result;
wire [31:0] ms_pc;

wire    [11:0]      ms_mem_control ;
wire    [31:0]      ms_rt_value    ;

wire    [4:0]       ms_excode ;
wire    [31:0]      ms_badvaddr ;
wire   [7:0]    ms_cp0_addr  ;
wire                ms_ex ;
wire                ms_bd ;
wire                ms_inst_eret   ;
wire                ms_inst_syscall;
wire                ms_inst_mfc0   ;
wire                ms_inst_mtc0   ;

assign ms_eret = ms_valid & ms_inst_eret ;

wire        es_to_ms_data_ok;
wire [31:0] es_to_ms_data;
wire        ms_wait_mem;

assign {
        ms_wait_mem      ,   //199:199
        es_to_ms_data_ok       ,   //198:198
        es_to_ms_data          ,   //197:166
        ms_excode        ,  //165:161
        ms_badvaddr      ,  //160:129
        ms_cp0_addr      ,  //128:121
        ms_ex            ,  //120:120
        ms_bd            ,  //119:119
        ms_inst_eret     ,  //118:118
        ms_inst_syscall  ,  //117:117
        ms_inst_mfc0     ,  //116:116
        ms_inst_mtc0     ,  //115:115
        ms_rt_value      ,  //114:83
        ms_mem_control   ,  //82:71
        ms_res_from_mem,  //70:70
        ms_gr_we       ,  //69:69
        ms_dest        ,  //68:64
        ms_alu_result  ,  //63:32
        ms_pc             //31:0
       } = es_to_ms_bus_r;

wire [31:0] mem_result;
wire [31:0] ms_final_result;

wire    to_ms_ex ;
assign  to_ms_ex = ms_ex ;

assign  to_ws_ex = ms_ex ;

wire    [3:0]   ms_gr_strb  ;

assign ms_to_ws_bus = {
                       ms_excode       ,   //123:119
                       ms_badvaddr     ,   //118:87
                       ms_cp0_addr     ,   //86:79
                       to_ms_ex           ,   //78:78
                       ms_bd           ,   //77:77
                       ms_inst_eret    ,   //76:76
                       ms_inst_syscall ,   //75:75
                       ms_inst_mfc0    ,   //74:74
                       ms_inst_mtc0    ,   //73:73
                    //    ms_rt_value    ,  //101:70
                       ms_gr_strb       ,  //72:69
                       ms_dest        ,  //68:64
                       ms_final_result,  //63:32
                       ms_pc             //31:0
                      };

assign ms_ready_go    = ms_wait_mem ? ms_data_ok : 1'b1;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin;
assign ms_to_ws_valid = ms_valid && ms_ready_go && !ws_eret && !ws_ex ;
always @(posedge clk) begin
    if (reset) begin
        ms_valid <= 1'b0;
    end
    else if (ms_allowin) begin
        ms_valid <= es_to_ms_valid;
    end

    if (es_to_ms_valid && ms_allowin) begin
        es_to_ms_bus_r  <= es_to_ms_bus;
    end
end

assign ms_ex_o = ms_valid && ms_ex ;
assign ms_inst_mfc0_o = ms_valid && ms_inst_mfc0 ;

//load byte、load half word ----lb lbu-----
wire            load_sign_lb    ;
wire   [31:0]   mem_result_lb   ;
wire   [31:0]   mem_result_lbu  ;

assign  load_sign_lb =  (ms_alu_result[1:0] == 2'd0) ?  data_sram_rdata[7]  :
                        (ms_alu_result[1:0] == 2'd1) ?  data_sram_rdata[15] :
                        (ms_alu_result[1:0] == 2'd2) ?  data_sram_rdata[23] :
                                                        data_sram_rdata[31]     ;

assign  mem_result_lb[7:0] = (ms_alu_result[1:0] == 2'd0) ?  data_sram_rdata[7:0]   :
                             (ms_alu_result[1:0] == 2'd1) ?  data_sram_rdata[15:8]  :
                             (ms_alu_result[1:0] == 2'd2) ?  data_sram_rdata[23:16] :
                                                             data_sram_rdata[31:24]    ;
assign  mem_result_lb[31:8] = {24{load_sign_lb}} ;
assign  mem_result_lbu = {24'h0,mem_result_lb[7:0]} ;
//-------lh、lhu--------------------
wire            load_sign_lh    ;
wire   [31:0]   mem_result_lh   ;  
wire   [31:0]   mem_result_lhu  ; 

assign  load_sign_lh =  (ms_alu_result[1:0] == 2'b00) ?  data_sram_rdata[15]  :
                        (ms_alu_result[1:0] == 2'b10) ?  data_sram_rdata[31]  :     
                                                                1'b0            ;
assign  mem_result_lh[15:0] =  (ms_alu_result[1:0] == 2'b00) ?  data_sram_rdata[15:0]   :
                               (ms_alu_result[1:0] == 2'b10) ?  data_sram_rdata[31:16]  :     
                                                                16'd0                   ;  
assign  mem_result_lh[31:16] = {16{load_sign_lh}} ;
assign  mem_result_lhu  =   {16'h0,mem_result_lh[15:0]} ;
//-------------------lwl--------------------
wire    [31:0]      mem_result_lwl  ;

assign  mem_result_lwl  =   (ms_alu_result[1:0] == 2'b00)  ?  {data_sram_rdata[7:0] , ms_rt_value[23:0]}  :
                            (ms_alu_result[1:0] == 2'b01)  ?  {data_sram_rdata[15:0], ms_rt_value[15:0]}  :
                            (ms_alu_result[1:0] == 2'b10)  ?  {data_sram_rdata[23:0], ms_rt_value[ 7:0] }  :
                                                               data_sram_rdata[31:0]                      ;
//-------------------lwr--------------------
wire    [31:0]      mem_result_lwr  ;

assign  mem_result_lwr  =   (ms_alu_result[1:0] == 2'b00)  ?   data_sram_rdata[31:0]                        :
                            (ms_alu_result[1:0] == 2'b01)  ?  {ms_rt_value[31:24] , data_sram_rdata[31:8]}  :
                            (ms_alu_result[1:0] == 2'b10)  ?  {ms_rt_value[31:16] , data_sram_rdata[31:16]} :
                                                              {ms_rt_value[31:8] , data_sram_rdata[31:24]} ;                   ;

assign mem_result  =    (ms_mem_control == 12'b0000_0000_0100)  ?   mem_result_lb   :
                        (ms_mem_control == 12'b0000_0000_1000)  ?   mem_result_lbu  :
                        (ms_mem_control == 12'b0000_0001_0000)  ?   mem_result_lh   :
                        (ms_mem_control == 12'b0000_0010_0000)  ?   mem_result_lhu  :
                        (ms_mem_control == 12'b0000_0100_0000)  ?   mem_result_lwl  :
                        (ms_mem_control == 12'b0000_1000_0000)  ?   mem_result_lwr  :
                                                                    data_sram_rdata     ;

assign ms_final_result = ms_res_from_mem ? mem_result
                                         : ms_alu_result;

wire [ 1:0] mem_addr;
wire [31:0] mem_word;
wire [31:0] mem_left;
wire [31:0] mem_right;
wire [ 3:0] mem_left_strb;
wire [ 3:0] mem_right_strb;

reg         ms_data_buff_valid;
reg  [31:0] ms_data_buff;
wire        ms_data_ok;
wire [31:0] ms_data;

assign mem_addr = ms_alu_result[1:0];
assign mem_word = ms_data;

assign mem_left = 
    ( {32{mem_addr == 2'b00}} & {mem_word[ 7: 0], 24'b0} ) |
    ( {32{mem_addr == 2'b01}} & {mem_word[15: 0], 16'b0} ) |
    ( {32{mem_addr == 2'b10}} & {mem_word[23: 0],  8'b0} ) |
    ( {32{mem_addr == 2'b11}} &  mem_word[31: 0]         ) ;
assign mem_left_strb =
    ( {4{mem_addr == 2'b00}} & 4'b1000 ) |
    ( {4{mem_addr == 2'b01}} & 4'b1100 ) |
    ( {4{mem_addr == 2'b10}} & 4'b1110 ) |
    ( {4{mem_addr == 2'b11}} & 4'b1111 ) ;

assign mem_right = 
    ( {32{mem_addr == 2'b00}} &         mem_word[31: 0]  ) |
    ( {32{mem_addr == 2'b01}} & { 8'b0, mem_word[31: 8]} ) |
    ( {32{mem_addr == 2'b10}} & {16'b0, mem_word[31:16]} ) |
    ( {32{mem_addr == 2'b11}} & {24'b0, mem_word[31:24]} );
assign mem_right_strb = 
    ( {4{mem_addr == 2'b00}} & 4'b1111 ) |
    ( {4{mem_addr == 2'b01}} & 4'b0111 ) |
    ( {4{mem_addr == 2'b10}} & 4'b0011 ) |
    ( {4{mem_addr == 2'b11}} & 4'b0001 );


assign ms_gr_strb =
    (ms_mem_control == 12'b0000_0100_0000)? mem_left_strb :
    (ms_mem_control == 12'b0000_1000_0000)? mem_right_strb :
    {4{ms_gr_we}};

always @ (posedge clk) begin
    if (reset) begin
        ms_data_buff_valid  <= 1'b0;
        ms_data_buff        <= 32'h0;
    end else if (!ms_data_buff_valid && ms_valid && data_sram_data_ok && !ws_allowin) begin
        ms_data_buff_valid  <= 1'b1;
        ms_data_buff        <= data_sram_rdata;
    end else if (ws_allowin || ws_eret || ws_ex) begin
        ms_data_buff_valid  <= 1'b0;
        ms_data_buff        <= 32'h0;
    end
end

assign ms_data_ok   = es_to_ms_data_ok || ms_data_buff_valid || (ms_wait_mem && data_sram_data_ok);
assign ms_data =
    es_to_ms_data_ok ?      es_to_ms_data   :
    ms_data_buff_valid ?    ms_data_buff    :
    data_sram_rdata;

assign ms_data_waiting  = ms_valid && ms_wait_mem && !ms_data_ok;
assign ms_inst_unable   = !ms_valid || ms_data_buff_valid || es_to_ms_data_ok;


assign  MEM_dest = ms_dest & {5{ms_valid}} & {5{ms_gr_we}};
assign  MEM_result = ms_final_result ;

endmodule
