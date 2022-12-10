module mycpu_top(
    input         clk,
    input         resetn,
    // inst sram interface
    output        inst_sram_en,
    output [ 3:0] inst_sram_wen,
    output [31:0] inst_sram_addr,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata,
    // data sram interface
    output        data_sram_en,
    output [ 3:0] data_sram_wen,
    output [31:0] data_sram_addr,
    output [31:0] data_sram_wdata,
    input  [31:0] data_sram_rdata,
    // trace debug interface
    output [31:0] debug_wb_pc,
    output [ 3:0] debug_wb_rf_wen,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata
);
reg         reset;
always @(posedge clk) reset <= ~resetn;

wire         ds_allowin;
wire         es_allowin;
wire         ms_allowin;
wire         ws_allowin;
wire         fs_to_ds_valid;
wire         ds_to_es_valid;
wire         es_to_ms_valid;
wire         ms_to_ws_valid;
wire [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus;
wire [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus;
wire [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus;
wire [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus;
wire [`WS_TO_RF_BUS_WD -1:0] ws_to_rf_bus;
wire [`BR_BUS_WD       -1:0] br_bus;


wire        [4:0]       EXE_dest ;
wire        [4:0]       MEM_dest ;
wire        [4:0]       WB_dest  ;
wire                    es_load_op ;

wire        [31:0]      EXE_result ;
wire        [31:0]      MEM_result ;
wire        [31:0]      WB_result  ;

wire                    ds_is_branch;
wire       [31:0]       cp0_epc     ;
wire                    ws_eret     ;
wire                    ws_ex       ;
wire                    ws_ex1      ;
wire       [31:0]       cp0_status  ;
wire       [31:0]       cp0_cause   ;

wire                    es_inst_mfc0 ;
wire                    ms_inst_mfc0 ;
wire                    ws_inst_mfc0 ;
wire        [4:0]        ws_rf_dest  ;

wire                    ms_ex       ;
wire                    ms_eret     ;

// IF stage
if_stage if_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ds_allowin     (ds_allowin     ),
    //brbus
    .br_bus         (br_bus         ),
    //outputs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_bus   (fs_to_ds_bus   ),
    // inst sram interface
    .inst_sram_en   (inst_sram_en   ),
    .inst_sram_wen  (inst_sram_wen  ),
    .inst_sram_addr (inst_sram_addr ),
    .inst_sram_wdata(inst_sram_wdata),
    .inst_sram_rdata(inst_sram_rdata),

    .ds_is_branch   (ds_is_branch)  ,
    .cp0_epc        (cp0_epc),
    .ws_eret        (ws_eret) ,
    .ws_ex          (ws_ex) ,
    .ws_ex1         (ws_ex1)
);
// ID stage
id_stage id_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .es_allowin     (es_allowin     ),
    .ds_allowin     (ds_allowin     ),
    //from fs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_bus   (fs_to_ds_bus   ),
    //to es
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_bus   (ds_to_es_bus   ),
    //to fs
    .br_bus         (br_bus         ),
    //to rf: for write back
    .ws_to_rf_bus   (ws_to_rf_bus   ),
    .EXE_dest       (EXE_dest),
    .MEM_dest       (MEM_dest),
    .WB_dest        (WB_dest),
    .es_load_op     (es_load_op),

    .EXE_result     (EXE_result)    ,
    .MEM_result     (MEM_result)    ,
    .WB_result      (WB_result)     ,
    .es_inst_mfc0   (es_inst_mfc0),
    .ms_inst_mfc0   (ms_inst_mfc0),
    .ws_inst_mfc0   (ws_inst_mfc0),
    .ws_rf_dest     (ws_rf_dest),

    .ds_is_branch   (ds_is_branch)  ,
    .ws_eret        (ws_eret)       ,
    .ws_ex          (ws_ex)         ,
    .cp0_status     (cp0_status)    ,
    .cp0_cause      (cp0_cause) 
);
// EXE stage
exe_stage exe_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ms_allowin     (ms_allowin     ),
    .es_allowin     (es_allowin     ),
    //from ds
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_bus   (ds_to_es_bus   ),
    //to ms
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_bus   (es_to_ms_bus   ),
    // data sram interface
    .data_sram_en   (data_sram_en   ),
    .data_sram_wen  (data_sram_wen  ),
    .data_sram_addr (data_sram_addr ),
    .data_sram_wdata(data_sram_wdata),

    .EXE_dest       (EXE_dest),
    .es_load_op     (es_load_op),
    .EXE_result     (EXE_result)    ,

    .es_inst_mfc0_o (es_inst_mfc0),
    .ws_ex          (ws_ex),
    .ms_ex          (ms_ex),
    .ms_eret        (ms_eret),
    .ws_eret        (ws_eret)
);
// MEM stage
mem_stage mem_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ws_allowin     (ws_allowin     ),
    .ms_allowin     (ms_allowin     ),
    //from es
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_bus   (es_to_ms_bus   ),
    //to ws
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_bus   (ms_to_ws_bus   ),
    //from data-sram
    .data_sram_rdata(data_sram_rdata),

    .MEM_dest       (MEM_dest),
    .MEM_result     (MEM_result)    ,

    .ms_inst_mfc0_o (ms_inst_mfc0),
    .ws_ex          (ws_ex),
    .ws_eret        (ws_eret),
    .ms_ex_o        (ms_ex),
    .ms_eret        (ms_eret)

);
// WB stage
wb_stage wb_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ws_allowin     (ws_allowin     ),
    //from ms
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_bus   (ms_to_ws_bus   ),
    //to rf: for write back
    .ws_to_rf_bus   (ws_to_rf_bus   ),
    //trace debug interface
    .debug_wb_pc      (debug_wb_pc      ),
    .debug_wb_rf_wen  (debug_wb_rf_wen  ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata),

    .wb_dest            (WB_dest),
    .WB_result          (WB_result),

    .ws_inst_mfc0_o     (ws_inst_mfc0),
    .ws_rf_dest         (ws_rf_dest),

    .ws_eret            (ws_eret),
    .ws_ex_o              (ws_ex) ,
    .ws_ex1                 (ws_ex1) ,
    .cp0_epc            (cp0_epc)   ,
    .cp0_status         (cp0_status) ,
    .cp0_cause          (cp0_cause)
);

endmodule