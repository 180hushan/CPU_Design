`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/02/09 12:25:22
// Design Name: 
// Module Name: regs
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////



`include  "define.v"

module regs(
    input                   reset,
    input                   clk,

    input                   we_i,
    input       [ 7:0]      waddr_i,
    input       [31:0]      wdata_i,

    // input       [ 7:0]      raddr_i,
    // output reg  [31:0]      rdata_o,

    input       [ 5:0]      ext_int_in,

    output   wire           c0_status_bev,
    // output reg  [ 7:0]      c0_status_im ,
    output reg              c0_status_exl,
    output reg              c0_status_ie ,

    output  reg             c0_cause_bd  ,
    output  reg             c0_cause_ti  ,
    output  reg  [ 7:0]     c0_cause_ip  ,
    output  reg  [ 4:0]     c0_cause_excode ,

    output  reg  [31:0]     c0_epc          ,
    output  reg  [31:0]     c0_badvaddr     ,
    output  reg  [31:0]     c0_count        ,

    output  reg  [31:0]     c0_compare

    );

    wire            wb_ex ;
    wire            eret_flush ;
    wire            wb_bd  ;
    wire    [4:0]   wb_excode  ;
    wire    [31:0]  wb_pc       ;
    wire    [31:0]  wb_badvaddr ;

    // wire            c0_status_bev ;
    reg     [7:0]   c0_status_im  ;
    // reg             c0_status_exl ;
    // reg             c0_status_ie  ;

    // assign  c0_status_bev = 1'b1 ;

// always @(posedge clk) begin
//     if(we_i && (waddr_i == `CR_STATUS)) begin
//         c0_status_im <= wdata_i[15:8] ;
//     end
// end

always @(posedge clk) begin
    if(we_i==1'b1) begin
        c0_status_im  <= wdata_i[15:8] ;
    end
end

    always @(posedge clk) begin
        if(reset) begin
            c0_status_exl  <= 1'b0 ;
        end
        else if(wb_ex) begin
            c0_status_exl  <= 1'b1 ;
        end
        else if(eret_flush) begin
            c0_status_exl  <= 1'b0 ;
        end
        else if(we_i && waddr_i == `CR_STATUS) begin
            c0_status_exl  <= wdata_i[1] ;
        end
    end

    always @(posedge clk) begin
        if(reset) begin
            c0_status_ie  <= 1'b0 ;
        end
        else if(we_i && waddr_i == `CR_STATUS) begin
            c0_status_ie  <= wdata_i[0] ;
        end
    end

    // reg                 c0_cause_bd  ;
    // reg                 c0_cause_ti  ;
    wire                count_eq_compare ;
    // reg     [7:0]       c0_cause_ip  ;
    // reg     [4:0]       c0_cause_excode  ;

    always @(posedge clk) begin
        if(reset) begin
            c0_cause_bd  <=  1'b0  ;
        end
        else if(wb_ex && !c0_status_exl) begin
            c0_cause_bd  <=  wb_bd  ;
        end
    end

    always @(posedge clk) begin
        if(reset) begin
            c0_cause_ti  <=  1'b0 ;
        end
        else if(we_i && waddr_i == `CR_COMPARE) begin
            c0_cause_ti  <=  1'b0 ;
        end
        else if(count_eq_compare) begin
            c0_cause_ti  <=  1'b1  ;
        end
    end

    always @(posedge clk) begin
        if(reset) begin
            c0_cause_ip[7:2]  <=  6'h0 ;
        end
        else begin
            c0_cause_ip[7]  <=  ext_int_in[5] | c0_cause_ti ;
            c0_cause_ip[6:2] <= ext_int_in[4:0] ;
        end
    end

    always @(posedge clk) begin
        if(reset) begin
            c0_cause_ip[1:0]    <=  2'b00 ;
        end
        else if(we_i && waddr_i == `CR_CAUSE) begin
            c0_cause_ip[1:0]   <= wdata_i[9:8] ; 
        end
    end

    always @(posedge clk) begin
        if(reset) begin
            c0_cause_excode  <=  5'h0 ;
        end
        else if(wb_ex) begin
            c0_cause_excode  <= wb_excode ;
        end
    end

//--------------- EPC寄存器 ------------------
    // reg     [31:0]      c0_epc  ;

    always @(posedge clk) begin
        if(wb_ex && !c0_status_exl) begin
            c0_epc   <=  wb_bd ?  wb_pc - 32'h4 : wb_pc ;
        end
        else if(we_i && waddr_i == `CR_EPC) begin
            c0_epc   <= wdata_i ;
        end
    end

//--------------- BadVAddr寄存器 ------------------
    // reg     [31:0]      c0_badvaddr ;

    always @(posedge clk) begin
        if(wb_ex && wb_excode == `EX_ADEL) begin
            c0_badvaddr  <=  wb_badvaddr ;
        end
    end

//--------------- count寄存器 ------------------
    reg                 tick     ;
    // reg     [31:0]      c0_count ;

    always @(posedge clk) begin
        if(reset) begin
            tick    <=  1'b0 ;
        end
        else begin
            tick   <= ~tick ;
        end
    end

    always @(posedge clk) begin
        if(we_i && waddr_i == `CR_COUNT) begin
            c0_count   <=  wdata_i ;
        end
        else if(tick) begin
            c0_count   <=  c0_count  + 1'b1 ;
        end
    end
//--------------- compare寄存器 ------------------

   always @(posedge clk) begin
       if(we_i && waddr_i == `CR_COMPARE) begin
           c0_compare  <=  wdata_i ;
       end
   end

//--------------------寄存器读操作---------------

    // always @(*) begin
    //     if(reset) begin
    //         rdata_o  <= 32'h0 ;
    //     end
    //     else begin
    //         case(raddr_i) 
    //             `CR_
    //     end
    // end

endmodule

