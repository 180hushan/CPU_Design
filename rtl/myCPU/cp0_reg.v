`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/02/09 11:15:42
// Design Name: 
// Module Name: cp0_reg
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
`include "mycpu.h"

module cp0_reg(
    input                   reset,
    input                   clk,

    input                   wb_ex,
    input                   wb_bd ,
    input                   ws_eret,
    input       [4:0]       wb_excode,
    input       [31:0]      wb_pc ,
    input       [31:0]      wb_badvaddr,
    input       [ 5:0]      ext_int_in,

    input                   we_i,
    input       [ 7:0]      addr_i,
    input       [31:0]      wdata_i,
    output      [31:0]      rdata_o,

    // input       [ 7:0]      raddr_i,
    // output reg  [31:0]      rdata_o,

    

    output       [31:0]     cp0_status,
    output       [31:0]     cp0_cause ,
    output       [31:0]     cp0_epc   ,
    output       [31:0]     cp0_badvaddr ,
    output       [31:0]     cp0_count        ,

    output       [31:0]     cp0_compare

    );


    wire            c0_status_bev ;
    assign  c0_status_bev = 1'b1 ;
    
    reg     [7:0]   c0_status_im  ;
    always @(posedge clk) begin
        if(we_i && addr_i == `CP0_STATUS_ADDR) begin
            c0_status_im <= wdata_i[15:8] ;
        end
    end

    reg             c0_status_exl ;
    always @(posedge clk) begin
        if(reset) begin
            c0_status_exl  <= 1'b0 ;
        end
        else if(wb_ex) begin
            c0_status_exl  <= 1'b1 ;
        end
        else if(ws_eret) begin
            c0_status_exl  <= 1'b0 ;
        end
        else if(we_i && addr_i == `CP0_STATUS_ADDR) begin
            c0_status_exl  <= wdata_i[1] ;
        end
    end

    reg             c0_status_ie  ;
    always @(posedge clk) begin
        if(reset) begin
            c0_status_ie  <= 1'b0 ;
        end
        else if(we_i && addr_i == `CP0_STATUS_ADDR) begin
            c0_status_ie  <= wdata_i[0] ;
        end
    end

    assign cp0_status = {
                            9'h0,            //31:23
                            c0_status_bev,   //22:22
                            6'h0,            //21:16
                            c0_status_im,    //15:8
                            6'h0,            //7:2
                            c0_status_exl,   //1:1
                            c0_status_ie    //0:0
                        }   ;

    reg                 c0_cause_bd  ;
    always @(posedge clk) begin
        if(reset) begin
            c0_cause_bd  <=  1'b0  ;
        end
        else if(wb_ex && !c0_status_exl) begin
            c0_cause_bd  <=  wb_bd  ;
        end
    end

    reg                 c0_cause_ti  ;
    wire                count_eq_compare ;
    assign count_eq_compare = (cp0_count == cp0_compare) ;

    always @(posedge clk) begin
        if(reset) begin
            c0_cause_ti  <=  1'b0 ;
        end
        else if(we_i && addr_i == `CP0_COMPARE_ADDR) begin
            c0_cause_ti  <=  1'b0 ;
        end
        else if(count_eq_compare) begin
            c0_cause_ti  <=  1'b1  ;
        end
    end

    reg     [7:0]       c0_cause_ip  ;
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
        else if(we_i && addr_i == `CP0_CAUSE_ADDR) begin
            c0_cause_ip[1:0]   <= wdata_i[9:8] ; 
        end
    end

    reg     [4:0]       c0_cause_excode  ;
    always @(posedge clk) begin
        if(reset) begin
            c0_cause_excode  <=  5'h0 ;
        end
        else if(wb_ex) begin
            c0_cause_excode  <= wb_excode ;
        end
    end

    assign cp0_cause = {
                            c0_cause_bd ,       //31:31
                            c0_cause_ti ,       //30:30
                            14'h0       ,       //29:16
                            c0_cause_ip ,       //15:8
                            1'b0        ,       //7:7
                            c0_cause_excode,    //6:2
                            2'b00              //1:0
                        } ;

//--------------- EPC寄存器 ------------------
    reg     [31:0]      c0_epc  ;

    always @(posedge clk) begin
        if(wb_ex && !c0_status_exl) begin
            c0_epc   <=  wb_bd ?  wb_pc - 32'h4 : wb_pc ;
        end
        else if(we_i && addr_i == `CP0_EPC_ADDR) begin
            c0_epc   <= wdata_i ;
        end
    end

    assign cp0_epc = c0_epc ;

//--------------- BadVAddr寄存器 ------------------
    reg     [31:0]      c0_badvaddr ;

    always @(posedge clk) begin
        if(wb_ex &&( wb_excode == `EX_ADEL || wb_excode == `EX_ADES)) begin
            c0_badvaddr  <=  wb_badvaddr ;
        end
    end

    assign cp0_badvaddr = c0_badvaddr ;

//--------------- count寄存器 ------------------
    reg                 tick     ;

    always @(posedge clk) begin
        if(reset) begin
            tick    <=  1'b0 ;
        end
        else begin
            tick   <= ~tick ;
        end
    end

    reg     [31:0]      c0_count ;
    always @(posedge clk) begin
        if(we_i && addr_i == `CP0_COUNT_ADDR) begin
            c0_count   <=  wdata_i ;
        end
        else if(tick) begin
            c0_count   <=  c0_count  + 1'b1 ;
        end
    end
    
    assign cp0_count = c0_count ;
//--------------- compare寄存器 ------------------
    reg     [31:0]  c0_compare ;
   always @(posedge clk) begin
       if(we_i && addr_i == `CP0_COMPARE_ADDR) begin
           c0_compare  <=  wdata_i ;
       end
   end

   assign cp0_compare = c0_compare ;

//--------------------寄存器读操作---------------

    assign rdata_o = 
        (addr_i == `CP0_STATUS_ADDR) ?  cp0_status   :
        (addr_i == `CP0_CAUSE_ADDR)  ?  cp0_cause    :
        (addr_i == `CP0_EPC_ADDR)    ?  cp0_epc      :
        (addr_i == `CP0_BADV_ADDR)    ?  cp0_badvaddr      :
        (addr_i == `CP0_COUNT_ADDR)    ?  cp0_count      :
        (addr_i == `CP0_COMPARE_ADDR)    ?  cp0_compare      :
        32'h0 ;
        

endmodule
