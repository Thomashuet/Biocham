package fr.inria.contraintes.biocham.customComponents;
import fr.inria.contraintes.biocham.plotting.PlotDataCSV;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;


/**
 * Class that holds the data of the simulation result plot in a table od a dataset name and their set of (x,y) values.
 * 
 * @author Dragana Jovanovska  
 */ 

public class PlotDataTable extends JScrollPane {
	   
		JTable table;
		ArrayList values,names;

		Float[][] data;    // data and time can be accessed by repaint()!
		String[] name;

		public PlotDataTable(BiochamPlot bp) {
	    
			
			super();
			PlotDataCSV plotData=bp.getPlotData();
			name=new String[plotData.getItems().size()+1];
			name[0]=plotData.getXAxisLabel();
			for(int i=0;i<plotData.getItems().size();i++){
				name[i+1]=plotData.getItems().get(i);
			}
			data=new Float[plotData.getItems().size()+1][plotData.getXTicks().size()];
			for(int i=0;i<plotData.getXTicks().size();i++){
				data[0][i]=Float.parseFloat(plotData.getXTicks().get(i));	
			}
			for(int i=0;i<plotData.getItems().size();i++){
				for(int j=0;j<plotData.getItemsYValues(plotData.getItems().get(i)).size();j++){
					data[i][j]=Float.parseFloat(plotData.getItemsYValues(plotData.getItems().get(i)).get(j));
				}
			}
			
			TableModel dataModel = new AbstractTableModel () {
	        
				public String getColumnName(int col) {
					return name[col];
				}			
				public int getRowCount() {
					return data[0].length;
				}
				public int getColumnCount() {
					return data.length;
				}
				public Object getValueAt(int row, int col) {
					return data[col][data[0].length-row-1];
				}
				public boolean isCellEditable(int row, int col) {
					return false;
				}
				public void setValueAt(Object value, int row, int col) {}
	      
			};			
			table = new JTable(dataModel);			
			table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);//.setAutoResizeMode(true);
			table.setGridColor(Utils.backgroundColor);
			//table.getTableHeader().setForeground(Color.BLUE);
			table.setBackground(Color.WHITE);
			table.setCellSelectionEnabled(true);
			table.setColumnSelectionAllowed(true);
			table.setRowSelectionAllowed(true);
			JLabel l=(JLabel)table.getTableHeader().getDefaultRenderer();			
			l.setPreferredSize(new Dimension(0,30));
			table.getTableHeader().setFont(Utils.menuBarFont);
			//table.setGridColor(Color.YELLOW);
			//table.set
			//table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			setFocusable(false);
			setViewportView(table);
			validate();
			table.setRowSelectionAllowed(false);
			table.setColumnSelectionAllowed(true);
			table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			
			
		}
	

}
