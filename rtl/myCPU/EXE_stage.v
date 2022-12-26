`include "mycpu.h"

module exe_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          ms_allowin    ,
    output                         es_allowin    ,
    //from ds
    input                          ds_to_es_valid,
    input  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus  ,
    //to ms
    output                         es_to_ms_valid,
    output [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus  ,
    // data sram interface
    // output        data_sram_en   ,
    // output [ 3:0] data_sram_wen  ,
    // output [31:0] data_sram_addr ,
    // output [31:0] data_sram_wdata,

    // from ms
    input                          ms_inst_unable,

    output          data_sram_req,
    output          data_sram_wr,
    output  [ 1:0]  data_sram_size,
    output  [31:0]  data_sram_wdata,
    output  [ 3:0]  data_sram_wstrb,
    output  [31:0]  data_sram_addr,
    input           data_sram_addr_ok,
    input   [31:0]  data_sram_rdata,
    input           data_sram_data_ok,
    output          es_data_waiting, 

    output      [4:0]       EXE_dest    ,
    output                  es_load_op  ,
    output      [31:0]      EXE_result  ,

    //block
    output                  es_inst_mfc0_o,

    input                   ws_ex       ,
    input                   ms_ex       ,
    input                   ms_eret     ,
    input                   ws_eret 
);

reg         es_valid      ;
wire        es_ready_go   ;

reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;
wire [11:0] es_alu_op     ;
// wire        es_load_op    ;
wire        es_src1_is_sa     ;  
wire        es_src1_is_pc     ;
wire        es_src2_is_imm    ; 
wire        es_src2_is_8      ;
wire        es_gr_we          ;
wire        es_mem_we         ;
wire [ 4:0] es_dest           ;
wire [15:0] es_imm            ;
wire [31:0] es_rs_value       ;
wire [31:0] es_rt_value       ;
wire [31:0] es_pc             ;
wire        es_src2_is_imm_zero_expand ;

wire   [3:0]      mult_div ;
wire   [3:0]      mf_mt    ;

//load sw指令控制信号
wire    [11:0]      es_mem_control ;

wire    [4:0]       es_excode ;
wire    [31:0]      es_badvaddr ;
wire   [7:0]        es_cp0_addr  ;
wire                es_ex ;
wire                es_bd ;
wire                es_inst_eret   ;
wire                es_inst_syscall;
wire                es_inst_mfc0   ;
wire                es_inst_mtc0   ;

wire    [4:0]       ds_to_es_excode ;
wire    [31:0]      ds_to_es_badvaddr ;  
wire                ds_to_es_ex ;
wire                ds_to_es_bd ;           

assign {
        fs_to_ds_ex      ,  //209:209
        overflow_inst    ,  //208:208
        ds_to_es_excode  ,  //207:203
        ds_to_es_badvaddr,  //202:171
        es_cp0_addr      ,  //170:163
        ds_to_es_ex      ,  //162:162
        ds_to_es_bd      ,  //161:161
        es_inst_eret     ,  //160:160
        es_inst_syscall  ,  //159:159
        es_inst_mfc0     ,  //158:158
        es_inst_mtc0     ,  //157:157
        es_mem_control  ,  //156:145
        mf_mt           , //144:141
        mult_div        , //140:137
        es_src2_is_imm_zero_expand, //136:136
        es_alu_op      ,  //135:124
        es_load_op     ,  //123:123
        es_src1_is_sa  ,  //122:122
        es_src1_is_pc  ,  //121:121
        es_src2_is_imm ,  //120:120
        es_src2_is_8   ,  //119:119
        es_gr_we       ,  //118:118
        es_mem_we      ,  //117:117
        es_dest        ,  //116:112
        es_imm         ,  //111:96
        es_rs_value    ,  //95 :64
        es_rt_value    ,  //63 :32
        es_pc             //31 :0
       } = ds_to_es_bus_r;

wire [31:0] es_alu_src1   ;
wire [31:0] es_alu_src2   ;
wire [31:0] es_alu_result ;

wire        [31:0]      es_result  ;
assign  es_result = (mf_mt[0] == 1'b1) ? lo             :
                    (mf_mt[1] == 1'b1) ? hi             :
                    (es_inst_mtc0)     ? es_rt_value    :
                                es_alu_result ;


wire        es_res_from_mem  ;

assign es_res_from_mem = es_load_op     ;

wire        no_store ;
assign no_store = ms_ex | ws_ex | es_ex | ms_eret | ws_eret ;

assign es_to_ms_bus = {
                       es_wait_mem      ,   //226:226
                       es_data_ok       ,   //225:225
                       es_data          ,   //224:193
                       es_excode        ,  //192:161
                       es_badvaddr      ,  //160:129
                       es_cp0_addr      ,  //128:121
                       es_ex            ,  //120:120
                       es_bd            ,  //119:119
                       es_inst_eret     ,  //118:118
                       es_inst_syscall  ,  //117:117
                       es_inst_mfc0     ,  //116:116
                       es_inst_mtc0     ,  //115:115
                       es_rt_value      ,  //114:83
                       es_mem_control   ,  //82:71
                       es_res_from_mem  ,  //70:70
                       es_gr_we         ,  //69:69
                       es_dest          ,  //68:64
                       es_result        ,  //63:32
                       es_pc             //31:0
                      };

// assign es_ready_go    = 1'b1;
// assign es_ready_go    = !ws_eret & !ws_ex & es_valid & (mult_div[3:2] == 2'b00 | div_ready_i) ;
// assign es_ready_go    =  es_valid & (((mult_div[3:2] == 2'b00) | div_ready_i) & !ws_ex)   ;
// assign es_ready_go    =  (mult_div[3:2] == 2'b00) && !ws_ex && es_valid ?   div_ready_i :
//                             1'b1 ;
// assign es_ready_go    =  es_valid & ((((mult_div[3:2] == 2'b00) | div_ready_i) | ((mult_div[3:2] == 2'b10) & ws_ex) | ws_eret) | ((es_mem_we || es_mem_re ) & es_addr_ok || es_ex))   ;
assign es_ready_go    =  (ws_ex || ws_eret)   ?   1'b1 :
                         (mult_div[3:2] == 2'b01) && !ws_ex && !ws_eret ?   div_ready_i :
                         (mult_div[3:2] == 2'b10) && !ws_ex && !ws_eret ?   div_ready_i :
                         (es_mem_we || es_mem_re )                      ?   es_addr_ok || es_ex :   
                            1'b1 ;
assign es_allowin     = !es_valid || es_ready_go && ms_allowin ;
assign es_to_ms_valid =  es_valid && es_ready_go && !ws_eret && !ws_ex ;
always @(posedge clk) begin
    if (reset) begin
        es_valid <= 1'b0;
    end
    else if (es_allowin) begin
        es_valid <= ds_to_es_valid;
    end

    if (ds_to_es_valid && es_allowin) begin
        ds_to_es_bus_r <= ds_to_es_bus ;
    end
end

assign es_alu_src1 = es_src1_is_sa  ? {27'b0, es_imm[10:6]} : 
                     es_src1_is_pc  ? es_pc[31:0] :
                                      es_rs_value;
assign es_alu_src2 = es_src2_is_imm_zero_expand ? {{16{1'b0}},es_imm[15:0]} :
                     es_src2_is_imm ? {{16{es_imm[15]}}, es_imm[15:0]} : 
                     es_src2_is_8   ? 32'd8 :
                                      es_rt_value;

alu u_alu(
    .alu_op     (es_alu_op    )     ,
    .alu_src1   (es_alu_src1  )     ,
    .alu_src2   (es_alu_src2  )     ,
    .alu_result (es_alu_result)     ,
    .overflow   (overflow)
    );

/******************{乘法部件 begin}***********************/

wire        [31:0]      operater1_mult;
wire        [31:0]      operater2_mult;
wire        [63:0]      mult_temp ;
reg         [63:0]      mult_result ;

assign  operater1_mult = ((mult_div[0] == 1'b1) && (es_alu_src1[31] == 1'b1)) ? 
                            (~es_alu_src1 + 1)  : es_alu_src1 ;
assign  operater2_mult = ((mult_div[0] == 1'b1) && (es_alu_src2[31] == 1'b1)) ? 
                            (~es_alu_src2 + 1)  : es_alu_src2 ;

assign  mult_temp  =  operater1_mult * operater2_mult ;

always @(*) begin
    if(reset) begin
        mult_result <= 64'd0 ;
    end
    else if(mult_div[0] == 1'b1) begin
        if(es_alu_src1[31] ^ es_alu_src2[31] == 1'b1) begin
            mult_result  <=  ~mult_temp + 1 ;
        end
        else begin
            mult_result  <= mult_temp ;
        end
    end
    else begin
        mult_result  <= mult_temp ;
    end
end
/******************{乘法部件 end}***********************/

/******************{除法部件 begin}***********************/

wire    [63:0]      div_result_i ;
wire                div_ready_i  ;
wire                div_start_o  ;
wire                signed_div_o ;

assign  div_start_o = ((mult_div[3:2] != 2'b00) && (div_ready_i == 1'b0)) ? 1'b1  : 1'b0 ;
assign  signed_div_o = (mult_div[2] == 1'b1)    ? 1'b1 :
                        (mult_div[3] == 1'b1)   ? 1'b0 : 1'b0   ;

div  sign_div (
    .clk(clk),
    .rst(~reset),
    .signed_div_i(signed_div_o),
    .opdata1_i(es_alu_src1),
    .opdata2_i(es_alu_src2),
    .start_i(div_start_o),
    .annul_i(1'b0),
    .result_o(div_result_i),
    .ready_o(div_ready_i)
);

/******************{除法部件 end}***********************/

/******************{HI/LO寄存器 begin}***********************/

reg     [31:0]      hi ;
reg     [31:0]      lo ;

always @(posedge clk) begin
    if(mult_div != 4'b0000 | mf_mt[2] == 1'b1) begin
        if(mult_div[1:0] != 2'b00 && !no_store && es_valid) begin
            lo  <=  mult_result[31:0] ;
        end
        else if(mult_div[3:2] != 2'b00 && !no_store && div_ready_i) begin
            lo  <= div_result_i[31:0] ;
        end
        else if(mf_mt[2]) begin
            lo  <= es_alu_src1 ;
        end
    end

    if(mult_div != 4'b0000 | mf_mt[3] == 1'b1) begin
        if(mult_div[1:0] != 2'b00 && !no_store && es_valid) begin
            hi  <=  mult_result[63:32] ;
        end
        else if(mult_div[3:2] != 2'b00 && !no_store && div_ready_i) begin
            hi  <= div_result_i[63:32] ;
        end
        else if(mf_mt[3]) begin
            hi  <= es_alu_src1 ;
        end 
    end
end

/******************{HI/LO寄存器 end}***********************/
wire        inst_is_lw  ;
wire        inst_is_sw  ;
wire        inst_is_lb  ;
wire        inst_is_lbu  ;
wire        inst_is_lh  ;
wire        inst_is_lhu  ;
wire        inst_is_lwl  ;
wire        inst_is_lwr  ;
wire        inst_is_sb  ;
wire        inst_is_sh  ;
wire        inst_is_swl ;
wire        inst_is_swr ;

wire        [3:0]       sram_wen       ;
wire        [31:0]      write_mem_data ;

assign  inst_is_lw  =  (es_mem_control == 12'b0000_0000_0001)   ;
assign  inst_is_sw  =  (es_mem_control == 12'b0000_0000_0010)   ;
assign  inst_is_lb  =  (es_mem_control == 12'b0000_0000_0100)   ;
assign  inst_is_lbu =  (es_mem_control == 12'b0000_0000_1000)   ;
assign  inst_is_lh  =  (es_mem_control == 12'b0000_0001_0000)   ;
assign  inst_is_lhu =  (es_mem_control == 12'b0000_0010_0000)   ;
assign  inst_is_lwl =  (es_mem_control == 12'b0000_0100_0000)   ;
assign  inst_is_lwr =  (es_mem_control == 12'b0000_1000_0000)   ;
assign  inst_is_sb  =  (es_mem_control == 12'b0001_0000_0000) ;
assign  inst_is_sh  =  (es_mem_control == 12'b0010_0000_0000) ;
assign  inst_is_swl =  (es_mem_control == 12'b0100_0000_0000) ;
assign  inst_is_swr =  (es_mem_control == 12'b1000_0000_0000) ;


assign  write_mem_data  =  inst_is_sb   ?  (es_alu_result[1:0] == 2'b00 ? {24'd0 , es_rt_value[7:0]}         : 
                                           es_alu_result[1:0] == 2'b01 ? {16'd0 , es_rt_value[7:0] , 8'd0}  : 
                                           es_alu_result[1:0] == 2'b10 ? {8'd0 , es_rt_value[7:0]  , 16'd0} :
                                                                         {es_rt_value[7:0] , 24'd0} 
                                           )    :    
                            inst_is_sh  ?  (es_alu_result[1:0] == 2'b00  ?  {16'd0 , es_rt_value[15:0]}     : 
                                                                            {es_rt_value[15:0] , 16'd0}
                                            )   :
                            inst_is_swl ?  (es_alu_result[1:0] == 2'b00  ?  {24'd0 , es_rt_value[31:24]}    :
                                            es_alu_result[1:0] == 2'b01  ?  {16'd0 , es_rt_value[31:16]}    :
                                            es_alu_result[1:0] == 2'b10  ?  {8'd0  , es_rt_value[31:8]}     :
                                                                            {es_rt_value[31:0]}
                                            )   :
                            inst_is_swr ?  (es_alu_result[1:0] == 2'b00  ?  {es_rt_value[31:0]}             :
                                            es_alu_result[1:0] == 2'b01  ?  {es_rt_value[23:0] , 8'd0}      :
                                            es_alu_result[1:0] == 2'b10  ?  {es_rt_value[15:0] , 16'd0}     :
                                                                            {es_rt_value[7:0] , 24'd0}
                                            )   :
                                            es_rt_value ;

assign  sram_wen  =  inst_is_sb  ?  (es_alu_result[1:0] == 2'b00  ?  4'b0001  :
                                     es_alu_result[1:0] == 2'b01  ?  4'b0010  :
                                     es_alu_result[1:0] == 2'b10  ?  4'b0100  :
                                                                     4'b1000
                                    )   :
                     inst_is_sh  ?  (es_alu_result[1:0] == 2'b00  ?  4'b0011  :
                                                                     4'b1100
                                    )   :
                     inst_is_swl ?  (es_alu_result[1:0] == 2'b00  ?  4'b0001  :
                                     es_alu_result[1:0] == 2'b01  ?  4'b0011  :
                                     es_alu_result[1:0] == 2'b10  ?  4'b0111  :
                                                                     4'b1111
                                    )   :
                     inst_is_swr ?  (es_alu_result[1:0] == 2'b00  ?  4'b1111  :
                                     es_alu_result[1:0] == 2'b01  ?  4'b1110  :
                                     es_alu_result[1:0] == 2'b10  ?  4'b1100  :
                                                                     4'b1000
                                    )   :
                                    4'b1111     ;

wire [ 1:0] st_addr;

assign st_addr = es_alu_result[1:0];

wire [ 1:0] lwl_swl_size;
wire [ 1:0] lwr_swr_size;

assign lwl_swl_size =
    ( {2{st_addr == 2'h0}} & 2'h0 ) |
    ( {2{st_addr == 2'h1}} & 2'h1 ) |
    ( {2{st_addr == 2'h2}} & 2'h2 ) |
    ( {2{st_addr == 2'h3}} & 2'h2 );

assign lwr_swr_size =
    ( {2{st_addr == 2'h0}} & 2'h2 ) |
    ( {2{st_addr == 2'h1}} & 2'h2 ) |
    ( {2{st_addr == 2'h2}} & 2'h1 ) |
    ( {2{st_addr == 2'h3}} & 2'h0 );

wire        es_mem_re     ;

assign  es_mem_re   =   inst_is_lw | inst_is_lh | inst_is_lhu | inst_is_lb | inst_is_lbu | inst_is_lwl | inst_is_lwr;

// assign data_sram_en    = 1'b1;
// assign data_sram_wen   =  (es_mem_we && !no_store && es_valid) ? sram_wen : 4'h0;
// assign data_sram_addr  = es_alu_result;
// assign data_sram_wdata = write_mem_data;

assign data_sram_req = 
    es_valid &&
    // ms_allowin &&
    !es_addr_ok_r && 
    (es_mem_we || es_mem_re) && !no_store;

assign data_sram_wr     = es_mem_we;

assign data_sram_size   =
    ( {2{inst_is_lw || inst_is_sw}}                 & 2'h2          ) |
    ( {2{inst_is_lh || inst_is_lhu || inst_is_sh}}  & 2'h1          ) |
    ( {2{inst_is_lb || inst_is_lbu || inst_is_sb}}  & 2'h0          ) |
    ( {2{inst_is_lwl || inst_is_swl}}               & lwl_swl_size  ) |
    ( {2{inst_is_lwr || inst_is_swr}}               & lwr_swr_size  );
assign data_sram_addr   =
    (inst_is_lwl || inst_is_swl) ? {es_alu_result[31:2], 2'b0} : es_alu_result[31:0];

wire        es_data_sram_data_ok;
reg         es_addr_ok_r;
wire        es_addr_ok;

reg         es_data_buff_valid;
reg  [31:0] es_data_buff;

wire        es_data_ok;
wire [31:0] es_data;

wire    es_wait_mem; 

assign es_data_waiting = es_valid && es_addr_ok && !es_data_ok;

assign es_data_sram_data_ok = data_sram_data_ok && ms_inst_unable;

assign data_sram_wdata  = write_mem_data;
assign data_sram_wstrb  = sram_wen;

always @ (posedge clk) begin
    if (reset) begin
        es_addr_ok_r <= 1'b0;
    end else if (data_sram_req && data_sram_addr_ok && !ms_allowin) begin
        es_addr_ok_r <= 1'b1;
    end else if (ms_allowin) begin
        es_addr_ok_r <= 1'b0;
    end
end
assign es_addr_ok   = (data_sram_req && data_sram_addr_ok) || es_addr_ok_r;

always @ (posedge clk) begin
    if (reset) begin
        es_data_buff_valid  <= 1'b0;
        es_data_buff        <= 32'h0;
    end else if (ms_allowin || no_store) begin
        es_data_buff_valid  <= 1'b0;
        es_data_buff        <= 32'h0;
    end else if (es_addr_ok && es_data_sram_data_ok && !ms_allowin) begin
        es_data_buff_valid  <= 1'b1;
        es_data_buff        <= data_sram_rdata;
    end
end
assign es_data_ok   = es_data_buff_valid || (es_addr_ok && es_data_sram_data_ok);
assign es_data =
    es_data_buff_valid ?    es_data_buff :
    data_sram_rdata;

assign es_wait_mem = es_valid && es_addr_ok;

assign es_inst_mfc0_o = es_valid && es_inst_mfc0 ;

wire  overflow_ex ;
wire    load_ex ;
wire    store_ex ;
wire  mem_ex ;

assign overflow_ex = overflow && overflow_inst ;

assign load_ex = (inst_is_lw && (es_alu_result[1:0] != 2'b00)) || ((inst_is_lh || inst_is_lhu) && (es_alu_result[0] != 1'b0)) ;
assign store_ex = (inst_is_sw && (es_alu_result[1:0] != 2'b00)) || (inst_is_sh && (es_alu_result[0] != 1'b0)) ;
assign mem_ex = load_ex || store_ex ;

assign es_ex  = (overflow_ex | mem_ex | ds_to_es_ex) & es_valid ;
assign es_bd    = ds_to_es_bd ;
assign es_badvaddr = (fs_to_ds_ex) ? ds_to_es_badvaddr  : es_alu_result ;
assign es_excode  = (ds_to_es_ex)   ?   ds_to_es_excode     :
                    (overflow_ex)   ?   `EX_OV      :
                    (load_ex)       ?   `EX_ADEL    :
                    (store_ex)      ?   `EX_ADES    :
                        ds_to_es_excode     ;

assign  EXE_dest = es_dest & {5{es_valid}}& {5{es_gr_we}}  ;
assign  EXE_result = es_result ;

endmodule
