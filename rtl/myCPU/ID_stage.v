`include "mycpu.h"

module id_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          es_allowin    ,      //执行阶段流水线允许进入
    output                         ds_allowin    ,      //译码阶段流水线允许进入
    //from fs
    input                          fs_to_ds_valid,          //指令-地址总线有效
    input  [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus  ,          //指令-地址总线
    //to es
    output                         ds_to_es_valid,          
    output [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus  ,
    //to fs
    output [`BR_BUS_WD       -1:0] br_bus        ,      
    //to rf: for write back
    input  [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus ,    //写回-寄存器堆总线

    input         [4:0]         EXE_dest ,
    input         [4:0]         MEM_dest ,
    input         [4:0]         WB_dest ,

    input       es_load_op,

    //from ms
    input       ms_waiting_data     ,

    input       [31:0]          EXE_result,
    input       [31:0]          MEM_result,
    input       [31:0]          WB_result,

    output                      ds_is_branch ,

    input                       es_inst_mfc0 ,
    input                       ms_inst_mfc0 ,
    input                       ws_inst_mfc0 ,
    input        [4:0]          ws_rf_dest  ,

    input                       ws_eret,
    input                       ws_ex ,

    input       [31:0]          cp0_status,
    input       [31:0]          cp0_cause 

    // input       [`ES_FWD_BLK_BUS_WD -1:0] es_fwd_blk_bus,
    // input   [`MS_FWD_BLK_BUS_WD -1:0] ms_fwd_blk_bus,
);

reg         ds_valid   ;    //ds流水线阶段有效
wire        ds_ready_go;    //ds阶段准备好处理

wire [31                 :0] fs_pc;         
reg  [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus_r;    //指令+地址总线 64bit
assign fs_pc = fs_to_ds_bus[31:0];

wire [31:0] ds_inst;
wire [31:0] ds_pc  ;

wire        fs_to_ds_ex ;
assign {
        fs_to_ds_ex ,   //96:96
        ds_badvaddr ,   //95:64
        ds_inst,        //63:32
        ds_pc           //31:0
        } = fs_to_ds_bus_r;

wire        rf_we   ;       //
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;
assign {rf_we   ,  //37:37
        rf_waddr,  //36:32
        rf_wdata   //31:0
       } = ws_to_rf_bus;        //写回总线分解

// wire [ 3:0] es_fwd_valid;
// wire [ 4:0] es_rf_dest;
// wire [31:0] es_rf_data;
// wire        es_blk_valid;
// assign {
//     es_fwd_valid,   // 41:38
//     es_rf_dest,     // 37:33
//     es_rf_data,     // 32:1
//     es_blk_valid    // 0:0
// } = es_fwd_blk_bus;

wire        br_leaving_ds ;
wire        br_stall ;
wire        load_stall ;
wire    [31:0]      bd_pc   ;

assign  br_stall = (load_stall | branch_stall) & ds_valid ;
// assign  br_stall = br_taken & load_stall & ds_valid ;
// assign  br_stall = 1'b0 ;
assign  load_stall = (rs_wait & (((rs==EXE_dest) & es_load_op & !ws_ex & !ws_eret) | ((rs==MEM_dest) & ms_waiting_data))) ||
                        (rt_wait & (((rt==EXE_dest) & es_load_op & !ws_ex & !ws_eret) | ((rt==MEM_dest) & ms_waiting_data)))  ;

wire    branch_stall ;

assign  branch_stall = ds_is_branch & (rs_wait || rt_wait) & ds_valid  ;

assign ds_is_branch = (inst_beq || inst_bne || inst_jal || inst_jr ||
                       inst_bgez || inst_bgtz || inst_blez || inst_bltz ||
                       inst_bgezal || inst_bltzal || inst_j || inst_jalr) && ds_valid ;

wire        br_taken;
wire [31:0] br_target;

wire [11:0] alu_op;     //算术逻辑单元，12种算术、逻辑运算指令
wire        load_op;    //加载指令
wire        src1_is_sa;      
wire        src1_is_pc; 
wire        src2_is_imm;
wire        src2_is_8;
wire        res_from_mem;
wire        gr_we;
wire        mem_we;
wire [ 4:0] dest;
wire [15:0] imm;
wire [31:0] rs_value;
wire [31:0] rt_value;

wire        src2_is_imm_zero_expand ;
// wire        src2_is_imm_one_expand ;

wire    [3:0]       mult_div ;
wire    [3:0]       mf_mt  ;

//------------指令拆分-----------
wire [ 5:0] op;
wire [ 4:0] rs;
wire [ 4:0] rt;
wire [ 4:0] rd;
wire [ 4:0] sa;
wire [ 5:0] func;
wire [25:0] jidx;
wire [63:0] op_d;
wire [31:0] rs_d;
wire [31:0] rt_d;
wire [31:0] rd_d;
wire [31:0] sa_d;
wire [63:0] func_d;
//----------------初步19条指令标记--------------------
wire        inst_add ;
wire        inst_addu;
wire        inst_sub ;
wire        inst_subu;
wire        inst_slt;
wire        inst_slti;
wire        inst_sltu;
wire        inst_sltiu ;
wire        inst_and;
wire        inst_andi ;
wire        inst_or;
wire        inst_ori ;
wire        inst_xor;
wire        inst_xori ;
wire        inst_nor;
wire        inst_sll;
wire        inst_sllv ;
wire        inst_srl;
wire        inst_srlv ;
wire        inst_sra;
wire        inst_srav ;
wire        inst_addi ;
wire        inst_addiu;
wire        inst_lui;
wire        inst_lw;
wire        inst_sw;
wire        inst_beq;
wire        inst_bne;
wire        inst_jal;
wire        inst_jr;
wire        inst_mult ;
wire        inst_multu ;
wire        inst_div  ;
wire        inst_divu ;
wire        inst_mfhi ;
wire        inst_mflo ;
wire        inst_mthi ;
wire        inst_mtlo ;
wire        inst_bgez ;
wire        inst_bgtz ;
wire        inst_blez ;
wire        inst_bltz ;
wire        inst_bgezal ;
wire        inst_bltzal ;
wire        inst_j      ;
wire        inst_jalr   ;
wire        inst_lb     ;
wire        inst_lbu    ;
wire        inst_lh     ;
wire        inst_lhu    ;
wire        inst_lwl    ;
wire        inst_lwr    ;
wire        inst_sb     ;
wire        inst_sh     ;
wire        inst_swl    ;
wire        inst_swr    ;
wire        inst_mfc0   ;
wire        inst_mtc0   ;
wire        inst_syscall ;
wire        inst_eret   ;

wire        inst_break  ;


wire        dst_is_r31;  
wire        dst_is_rt;   

//---------------与寄存器堆regfiles交互-------------//
wire [ 4:0] rf_raddr1;      //读地址端口1
wire [31:0] rf_rdata1;      //读端口1数据  
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;

wire        rs_eq_rt;   //源操作数1 = 源操作数2情况 ，用于判断操作数相等时的跳转

assign br_bus       = {br_leaving_ds ,br_stall,br_taken,br_target};

wire    [4:0]       ds_excode ;
wire                ds_ex ;
wire    [7:0]       cp0_addr    ;
wire                ds_bd ;
wire    [31:0]       ds_badvaddr    ;

wire                overflow_inst ;
assign overflow_inst = inst_add || inst_sub || inst_addi ;

assign cp0_addr = {ds_inst[15:11],ds_inst[2:0]} ;

assign ds_to_es_bus = {
                       fs_to_ds_ex  ,   //209:209
                       overflow_inst,   //208:208
                       ds_excode    ,   //207:203
                       ds_badvaddr  ,   //202:171
                       cp0_addr     ,   //170:163
                       ds_ex        ,   //162:162
                       ds_is_bd     ,   //161:161
                       inst_eret    ,   //160:160
                       inst_syscall ,   //159:159 
                       inst_mfc0    ,   //158:158
                       inst_mtc0    ,   //157:157
                       mem_control ,    //156:145
                       mf_mt      ,     //144:141
                       mult_div   ,     //140:137
                       src2_is_imm_zero_expand, //136:136
                       alu_op      ,  //135:124
                       load_op     ,  //123:123
                       src1_is_sa  ,  //122:122
                       src1_is_pc  ,  //121:121
                       src2_is_imm ,  //120:120
                       src2_is_8   ,  //119:119
                       gr_we       ,  //118:118
                       mem_we      ,  //117:117
                       dest        ,  //116:112
                       imm         ,  //111:96
                       rs_value    ,  //95 :64
                       rt_value    ,  //63 :32
                       ds_pc          //31 :0
                      };

assign  mult_div = {inst_divu, inst_div, inst_multu, inst_mult};
assign  mf_mt    = {inst_mthi, inst_mtlo, inst_mfhi, inst_mflo};

// load sw指令控制
wire        [11:0]      mem_control ;
assign  mem_control  =  {inst_swr , inst_swl , inst_sh  , inst_sb ,
                         inst_lwr , inst_lwl , inst_lhu , inst_lh ,
                         inst_lbu , inst_lb  , inst_sw  , inst_lw  } ;



wire        src1_no_rs ;    //rs域存在，且非0，且不会从通用寄存器读数据，rs为目的寄存器情况
wire        src2_no_rt ;    //rt域存在，且非0，且不会从通用寄存器读数据,rt为目的寄存器情况
wire        no_rs_rt ;      //rs和rt域都不存在

assign  src1_no_rs = 1'b0 ;
assign  src2_no_rt = inst_addi | inst_addiu | inst_slti | inst_sltiu | inst_andi 
                     | inst_ori | inst_xori | load_op | inst_lui ;
assign  no_rs_rt  = inst_jal | inst_j ;

wire        rs_wait ;
wire        rt_wait ;

assign  rs_wait = ~src1_no_rs & (~no_rs_rt) & (rs != 5'd0) & ((rs == EXE_dest) | (rs == MEM_dest) | (rs == WB_dest)) ;
assign  rt_wait = ~src2_no_rt & (~no_rs_rt) & (rt != 5'd0) & ((rt == EXE_dest) | (rt == MEM_dest) | (rt == WB_dest)) ;

wire        inst_no_dest ;
assign   inst_no_dest = inst_beq  || inst_bne  || inst_bgez || inst_bgtz || 
                        inst_blez || inst_bltz || inst_jr   || inst_j    || 
                        inst_sw   || inst_sb   || inst_sh   || inst_swl  ||
                        inst_swr    ;

assign      src2_is_imm_zero_expand = src2_is_imm & (inst_andi | inst_ori | inst_xori) ;

wire        mfc0_block ;
assign  mfc0_block = (es_inst_mfc0 && (EXE_dest == rs || EXE_dest == rt)) ||
                    (ms_inst_mfc0 && (MEM_dest == rs || MEM_dest == rt)) ||
                    (ws_inst_mfc0 && (WB_dest == rs || WB_dest == rt)) ;

// assign ds_ready_go    = 1'b1;
// assign ds_ready_go    = ~rs_wait & ~rt_wait ; 
assign ds_ready_go    =  ~load_stall && ~mfc0_block ; 
assign ds_allowin     = !ds_valid || ds_ready_go && es_allowin;
assign ds_to_es_valid = ds_valid && ds_ready_go && !ws_eret && !ws_ex ;
always @(posedge clk) begin
    if (fs_to_ds_valid && ds_allowin) begin
        fs_to_ds_bus_r <= fs_to_ds_bus;
    end
end

always @(posedge clk) begin
    if(reset) begin
        ds_valid <= 1'b0 ;
    end
    else if(ds_allowin) begin
        ds_valid <= fs_to_ds_valid ;
    end
end

assign op   = ds_inst[31:26];   //指令码
assign rs   = ds_inst[25:21];       //源操作数1寄存器地址
assign rt   = ds_inst[20:16];       //源操作数2寄存器地址  
assign rd   = ds_inst[15:11];       //结果目的寄存器地址
assign sa   = ds_inst[10: 6];       //移位操作
assign func = ds_inst[ 5: 0];       //功能码
assign imm  = ds_inst[15: 0];       //立即数
assign jidx = ds_inst[25: 0]; 



//---------------译码器--------------------//
decoder_6_64 u_dec0(.in(op  ), .out(op_d  ));
decoder_6_64 u_dec1(.in(func), .out(func_d));
decoder_5_32 u_dec2(.in(rs  ), .out(rs_d  ));
decoder_5_32 u_dec3(.in(rt  ), .out(rt_d  ));
decoder_5_32 u_dec4(.in(rd  ), .out(rd_d  ));
decoder_5_32 u_dec5(.in(sa  ), .out(sa_d  ));

//------------------------判断为何种指令--------------------//
assign inst_add    = op_d[6'h00] & func_d[6'h20] & sa_d[5'h00];
assign inst_addu   = op_d[6'h00] & func_d[6'h21] & sa_d[5'h00];
assign inst_sub    = op_d[6'h00] & func_d[6'h22] & sa_d[5'h00];
assign inst_subu   = op_d[6'h00] & func_d[6'h23] & sa_d[5'h00];
assign inst_slt    = op_d[6'h00] & func_d[6'h2a] & sa_d[5'h00];
assign inst_slti   = op_d[6'h0a] ;
assign inst_sltu   = op_d[6'h00] & func_d[6'h2b] & sa_d[5'h00];
assign inst_sltiu  = op_d[6'h0b] ;
assign inst_and    = op_d[6'h00] & func_d[6'h24] & sa_d[5'h00];
assign inst_andi   = op_d[6'h0c] ;
assign inst_or     = op_d[6'h00] & func_d[6'h25] & sa_d[5'h00];
assign inst_ori    = op_d[6'h0d] ;
assign inst_xor    = op_d[6'h00] & func_d[6'h26] & sa_d[5'h00];
assign inst_xori   = op_d[6'h0e] ;
assign inst_nor    = op_d[6'h00] & func_d[6'h27] & sa_d[5'h00];
assign inst_sll    = op_d[6'h00] & func_d[6'h00] & rs_d[5'h00];
assign inst_sllv   = op_d[6'h00] & func_d[6'h04] & sa_d[5'h00];
assign inst_srl    = op_d[6'h00] & func_d[6'h02] & rs_d[5'h00];
assign inst_srlv   = op_d[6'h00] & func_d[6'h06] & sa_d[5'h00];
assign inst_sra    = op_d[6'h00] & func_d[6'h03] & rs_d[5'h00];
assign inst_srav   = op_d[6'h00] & func_d[6'h07] & sa_d[5'h00];
assign inst_addi   = op_d[6'h08] ;
assign inst_addiu  = op_d[6'h09];
assign inst_lui    = op_d[6'h0f] & rs_d[5'h00];
assign inst_lw     = op_d[6'h23];
assign inst_sw     = op_d[6'h2b];
assign inst_beq    = op_d[6'h04];
assign inst_bne    = op_d[6'h05];
assign inst_jal    = op_d[6'h03];
assign inst_jr     = op_d[6'h00] & func_d[6'h08] & rt_d[5'h00] & rd_d[5'h00] & sa_d[5'h00];
assign inst_mult   = op_d[6'h00] & func_d[6'h18] & sa_d[5'h00] & rd_d[5'h00] ;
assign inst_multu  = op_d[6'h00] & func_d[6'h19] & sa_d[5'h00] & rd_d[5'h00] ;
assign inst_div    = op_d[6'h00] & func_d[6'h1a] & sa_d[5'h00] & rd_d[5'h00] ;
assign inst_divu   = op_d[6'h00] & func_d[6'h1b] & sa_d[5'h00] & rd_d[5'h00] ;
assign inst_mfhi   = op_d[6'h00] & func_d[6'h10] & sa_d[5'h00] & rs_d[5'h00] & rt_d[5'h00] ;
assign inst_mflo   = op_d[6'h00] & func_d[6'h12] & sa_d[5'h00] & rs_d[5'h00] & rt_d[5'h00] ;
assign inst_mthi   = op_d[6'h00] & func_d[6'h11] & sa_d[5'h00] & rt_d[5'h00] & rd_d[5'h00] ;
assign inst_mtlo   = op_d[6'h00] & func_d[6'h13] & sa_d[5'h00] & rt_d[5'h00] & rd_d[5'h00] ;
assign inst_bgez   = op_d[6'h01] & rt_d[5'h01] ;
assign inst_bgtz   = op_d[6'h07] & rt_d[5'h00] ;
assign inst_blez   = op_d[6'h06] & rt_d[5'h00] ;
assign inst_bltz   = op_d[6'h01] & rt_d[5'h00] ;
assign inst_bgezal = op_d[6'h01] & rt_d[5'h11] ;
assign inst_bltzal = op_d[6'h01] & rt_d[5'h10] ;
assign inst_j      = op_d[6'h02] ;
assign inst_jalr   = op_d[6'h00] & func_d[6'h09] & sa_d[5'h00] & rt_d[5'h00] ;
assign inst_lb     = op_d[6'h20] ;
assign inst_lbu    = op_d[6'h24] ;
assign inst_lh     = op_d[6'h21] ;
assign inst_lhu    = op_d[6'h25] ;
assign inst_lwl    = op_d[6'h22] ;
assign inst_lwr    = op_d[6'h26] ;
assign inst_sb     = op_d[6'h28] ;
assign inst_sh     = op_d[6'h29] ;
assign inst_swl    = op_d[6'h2a] ;
assign inst_swr    = op_d[6'h2e] ;
assign inst_mfc0   = op_d[6'h10] & rs_d[5'h00] & sa_d[5'h00] & (ds_inst[5:3] == 3'h0) ;
assign inst_mtc0   = op_d[6'h10] & rs_d[5'h04] & sa_d[5'h00] & (ds_inst[5:3] == 3'h0) ;
assign inst_syscall = op_d[6'h00] & func_d[6'h0c] ;
assign inst_eret   = op_d[6'h10] & rs_d[5'h10] & rt_d[5'h00] & rd_d[5'h00] & sa_d[5'h00] & func_d[6'h18] ;
assign inst_break  = op_d[6'h00] & func_d[6'h0d]  ; 

assign alu_op[ 0] = inst_addi   | inst_add    | inst_addu | inst_addiu | 
                    inst_bgezal | inst_bltzal | inst_jalr | inst_lw    | 
                    inst_lb     | inst_lbu    | inst_lh   | inst_lhu   |
                    inst_lwl    | inst_lwr    | inst_sw   | inst_jal   |
                    inst_sb     | inst_sh     | inst_swl  | inst_swr    ;
assign alu_op[ 1] = inst_sub | inst_subu;
assign alu_op[ 2] = inst_slt | inst_slti ;
assign alu_op[ 3] = inst_sltu | inst_sltiu ;
assign alu_op[ 4] = inst_and | inst_andi ;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori ;
assign alu_op[ 7] = inst_xor | inst_xori ;
assign alu_op[ 8] = inst_sll | inst_sllv ;
assign alu_op[ 9] = inst_srl | inst_srlv ;
assign alu_op[10] = inst_sra | inst_srav ;
assign alu_op[11] = inst_lui;

assign src1_is_sa   = inst_sll   | inst_srl | inst_sra;
assign src1_is_pc   = inst_jal | inst_bgezal | inst_bltzal | inst_jalr ;

assign src2_is_imm  = inst_addi | inst_addiu | inst_slti | inst_sltiu |
                      inst_andi | inst_ori   | inst_xori | inst_lui   | 
                      inst_lw   | inst_lb    | inst_lbu  | inst_lh    |
                      inst_lhu  | inst_lwl   | inst_lwr  | inst_sw    |
                      inst_sb   | inst_sh    | inst_swl  | inst_swr   ;

assign src2_is_8    = inst_jal | inst_bgezal | inst_bltzal | inst_jalr ;

assign res_from_mem = inst_lw   | inst_lb    | inst_lbu  | inst_lh    |
                      inst_lhu  | inst_lwl   | inst_lwr;

assign dst_is_r31   = inst_jal | inst_bgezal | inst_bltzal ;

assign dst_is_rt    = inst_addi | inst_addiu | inst_slti | inst_sltiu |
                      inst_andi | inst_ori   | inst_xori | inst_lui   | 
                      inst_lw   | inst_lb    | inst_lbu  | inst_lh    |
                      inst_lhu  | inst_lwl   | inst_lwr  | inst_mfc0  ;     //针对ADDIU等，ADDIU，rs与扩展后立即数相加，存入rt中，即ADDIU没有rd

assign gr_we        = ~inst_sw   &  ~inst_beq  &  ~inst_bne  & ~inst_bgez & 
                      ~inst_bgtz &  ~inst_blez &  ~inst_bltz & ~inst_jr   & 
                      ~inst_j    &  ~inst_sb   &  ~inst_sh   & ~inst_swl  &
                      ~inst_swr  &  ~inst_mtc0 & ~inst_syscall & ~inst_eret & ~inst_break  ;

assign mem_we       = inst_sw | inst_sb  | inst_sh   | inst_swl  | inst_swr   ;

assign load_op = inst_lw   | inst_lb    | inst_lbu  | inst_lh    |
                 inst_lhu  | inst_lwl   | inst_lwr; ;



//-------------目的寄存器设置------------根据不同指令寄存器不同
assign dest         = dst_is_r31 ? 5'd31 :
                      dst_is_rt  ? rt    : 
                      inst_no_dest ? 5'd0 :
                                   rd;

assign rf_raddr1 = rs;
assign rf_raddr2 = rt;
regfile u_regfile(
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );

// assign rs_value = rf_rdata1;
// assign rt_value = rf_rdata2;

assign rs_value = rs_wait ? ((rs == EXE_dest) ? EXE_result :
                            (rs == MEM_dest) ? MEM_result : WB_result) :
                                            rf_rdata1 ;
assign rt_value = rt_wait ? ((rt == EXE_dest) ? EXE_result :
                            (rt == MEM_dest) ? MEM_result : WB_result) :
                                            rf_rdata2 ;

//-------------------跳转部分译码---------------//
assign rs_eq_rt = (rs_value == rt_value);

wire        rsbgez  ;
wire        rsbgtz  ;
wire        rsblez  ;
wire        rsbltz  ;

assign  rsbgez = (rs_value[31] == 1'b0 ) || (rs_value == 32'd0) ;
assign  rsbgtz = (rs_value[31] == 1'b0 ) && (rs_value != 32'd0) ;
assign  rsblez = (rs_value[31] == 1'b1 ) || (rs_value == 32'd0) ;
assign  rsbltz = (rs_value[31] == 1'b1 ) && (rs_value != 32'd0) ;

assign br_leaving_ds = br_taken & ds_ready_go & es_allowin ;

assign  bd_pc   =   ds_pc + 32'h4 ;

assign br_taken = (   inst_beq      &&  rs_eq_rt
                   || inst_bne      && !rs_eq_rt
                   || inst_bgez     && rsbgez
                   || inst_bgtz     && rsbgtz 
                   || inst_blez     && rsblez
                   || inst_bltz     && rsbltz 
                   || inst_bgezal   && rsbgez 
                   || inst_bltzal   && rsbltz 
                   || inst_j 
                   || inst_jalr 
                   || inst_jal
                   || inst_jr
                  ) && ds_valid;
assign br_target = (inst_beq || inst_bne || inst_bgez || inst_bgtz || inst_blez || inst_bltz || inst_bgezal || inst_bltzal) ? (bd_pc + {{14{imm[15]}}, imm[15:0], 2'b0}) :
                   (inst_jr || inst_jalr)              ? rs_value :
                  /*inst_jal*/              {bd_pc[31:28], jidx[25:0], 2'b0};

wire    ds_is_bd ;
reg     ds_is_bd_r ;

assign  ds_is_bd = ds_is_bd_r & ds_valid ;

always @(posedge clk ) begin
    if(reset) begin
        ds_is_bd_r  <=  1'b0 ;
    end
    else if(ws_eret || ws_ex) begin
        ds_is_bd_r  <=  1'b0 ;
    end
    else if(ds_to_es_valid && es_allowin) begin
        ds_is_bd_r  <=  ds_is_branch ;
    end
end

wire        interrupt ;
assign      interrupt = ((cp0_cause[15:8] & cp0_status[15:8]) != 8'b0) && (cp0_status[1:0] == 2'b01) ;

wire other_inst;
assign other_inst = !(inst_addu | inst_subu | inst_slt | inst_sltu | inst_and | inst_or | inst_xor | inst_nor
| inst_sll | inst_srl | inst_sra | inst_addiu | inst_lui | inst_lw | inst_sw | inst_beq | inst_bne | inst_jal
| inst_jr | inst_add | inst_addi | inst_sub | inst_slti | inst_sltiu | inst_andi | inst_ori | inst_xori | inst_sllv
| inst_srlv | inst_srav | inst_mult | inst_multu | inst_div | inst_divu | inst_mfhi | inst_mflo | inst_mthi | inst_mtlo
| inst_bgez | inst_bgtz | inst_blez | inst_bltz | inst_j | inst_bltzal | inst_bgezal | inst_jalr | inst_lb | inst_lbu
| inst_lh | inst_lhu | inst_lwl | inst_lwr | inst_sb | inst_sh | inst_swl | inst_swr | inst_syscall | inst_eret | inst_mfc0
| inst_mtc0 | inst_break);

assign ds_ex = (fs_to_ds_ex | inst_syscall | inst_break | other_inst | interrupt) & ds_valid ;

assign ds_excode =  (interrupt)    ?    `EX_INT     :
                    (fs_to_ds_ex)  ?    `EX_ADEL    :
                    (other_inst)   ?    `EX_RI   :
                    (inst_syscall) ?     `EX_SYS     :
                    (inst_break)   ?    `EX_BP      :
                                        `EX_NO ;

endmodule
