package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.WorkbenchToolBars;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;




public class DialogKinetics extends JFrame{
	
	
	
	JScrollPane p;
	JPanel kineticsPanel;
	
	public DialogKinetics(){
		
		super();
		this.setIconImage(Icons.images.get("SSicon.png"));	 
		setLocationRelativeTo(BiochamMainFrame.frame);
		this.setTitle("Kinetics");		
		super.setBackground(Utils.backgroundColor);		
		this.setBackground(Utils.backgroundColor);	
		getContentPane().setBackground(Utils.backgroundColor);
		kineticsPanel=new JPanel(new BorderLayout());
		kineticsPanel.setBackground(Utils.backgroundColor);
		
		
	}
	
	public void show(BiochamModel m){
		boolean nthYet=false;
		if(m!=null){
			if(m.getKinetics()!=null){		
				
				kineticsPanel.removeAll();
				this.getContentPane().removeAll();
				this.setTitle("Kinetics- "+m.getModelName());
				DefaultTableModel model = new DefaultTableModel();
				JTable area=new JTable(model);	
				area.setBackground(Utils.backgroundColor);
				area.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);		
				JLabel l=(JLabel)area.getTableHeader().getDefaultRenderer();			
				l.setPreferredSize(new Dimension(0,50));
				area.getTableHeader().setFont(Utils.menuBarFont);
				model.addColumn("Init");
				model.addColumn("ODE");										
				area.getColumnModel().getColumn(0).setPreferredWidth(50);
				area.getColumnModel().getColumn(1).setPreferredWidth(m.getMaxSizeOfKinetics()+50);				
				area.setRowHeight(25);				
				for(int i=0;i<m.getKinetics().size();i++){
					model.addRow(m.getKinetics().get(i));
				}	
				kineticsPanel.add(area.getTableHeader(), BorderLayout.NORTH);
				kineticsPanel.add(area,BorderLayout.CENTER);
				JScrollPane sp=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				sp.setAutoscrolls(true);
				sp.setViewportView(kineticsPanel);
				getContentPane().add(sp,BorderLayout.CENTER);
				 Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
				 this.setLocation(screenSize.width/3, screenSize.height/4);
				 pack();
				 this.setVisible(true);
			}else{
				nthYet=true;
			}
			
						
		}else{
			nthYet=true;
		}
		if(nthYet){
			WorkbenchToolBars.infoLabel.setText("You don't have any rule defined yet.....or try again");
    		WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);
		}
	}

	public JPanel getKineticsPanel() {
		return kineticsPanel;
	}

	public void setKineticsPanel(JPanel kineticsPanel) {
		this.kineticsPanel = kineticsPanel;
	} 
		
}