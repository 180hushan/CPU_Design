`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/02/09 11:31:41
// Design Name: 
// Module Name: test
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


module test(
    input clk,
    input we_i,
    input [7:0]     wdata_i,
    output reg [7:0]    c0_status_im

    );

    always @(posedge clk) begin
        if(we_i) begin
            c0_status_im <= wdata_i ;
        end
    end
endmodule
