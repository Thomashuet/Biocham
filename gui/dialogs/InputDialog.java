package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.customComponents.BrowseButton;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.StringTokenizer;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;




public class InputDialog extends JDialog{

	
	
	GradientPanel p;
	JTextField tf1,tf2;
	JComboBox cb;	
	JCheckBox checkb;
	String molecule=null,setOfObjectPatterns=null;
	boolean mergeOnly=false;
	JFrame parentFrame;
	BrowseButton file1, file2;
	
	public InputDialog(JFrame parent,String dialogName,ArrayList<String> listOfObjects,String property1,String property2){
	
		super(parent,dialogName);		
		setModal(true);				
		parentFrame=parent;
		p=new GradientPanel();		
		SpringLayout sl=new SpringLayout();
		p.setLayout(sl);	
		JLabel l1=new JLabel(property1);
		JLabel example=new JLabel();
		if(dialogName.equals("Merge Species")){
			example.setText("<html><u><i>Example:</i></u> Object pattern: <i>Cdc2-?</i> Molecule: <i>Cdc2</i></html>");
		}else if(dialogName.equals("Delete Species")){
			example.setText("<html><u><i>Example:</i></u> Object pattern: <i>Cdc2-?</i></html>");
		}else if(dialogName.equals("Merge Reactions")){
			example.setText("<html><u><i>Example:</i></u> Reaction pattern: <i>MAPK~? + MAPKK => MAPK + MAPKK</i></html>");
		}else if(dialogName.equals("Delete Reactions")){
			example.setText("<html><u><i>Example:</i></u> Reaction pattern: <i>MAPK~? + MAPKK => MAPK + MAPKK</i></html>");
		}
		tf1=new JTextField(10);
		p.add(l1);
		p.add(tf1);
		p.add(example);
		sl.putConstraint(SpringLayout.WEST,l1,10,SpringLayout.WEST,p);
		sl.putConstraint(SpringLayout.WEST,tf1,10,SpringLayout.EAST,l1);	
		sl.putConstraint(SpringLayout.NORTH,l1,20,SpringLayout.NORTH,p);
		sl.putConstraint(SpringLayout.NORTH,tf1,20,SpringLayout.NORTH,p);
		
		sl.putConstraint(SpringLayout.WEST,example,10,SpringLayout.WEST,p);
		sl.putConstraint(SpringLayout.NORTH,example,20,SpringLayout.SOUTH,l1);
		
		int height=180;
		int width=500;
		if(dialogName.equals("Delete Species")){
			width-=200;
		}else if(dialogName.equals("Merge Species") && listOfObjects!=null && listOfObjects.size()>0){
			width+=100;
		}
		if(property2!=null){
			
			JLabel l2=new JLabel(property2);
			
			if(property2.equals("File2:")){
				tf2=new JTextField(10);
				file1=new BrowseButton(parentFrame,tf1);				
				file2=new BrowseButton(parentFrame,tf2);				
				p.add(file1);
				p.add(l2);
				p.add(tf2);
				p.add(file2);
				checkb=new JCheckBox("Merge-only");
				checkb.setSelected(false);
				p.add(checkb);
				sl.putConstraint(SpringLayout.WEST,file1,10,SpringLayout.EAST,tf1);	
				sl.putConstraint(SpringLayout.WEST,l2,10,SpringLayout.WEST,p);
				sl.putConstraint(SpringLayout.WEST,tf2,10,SpringLayout.EAST,l2);
				sl.putConstraint(SpringLayout.WEST,file2,10,SpringLayout.EAST,tf2);	
				sl.putConstraint(SpringLayout.NORTH,file1,20,SpringLayout.NORTH,p);
				sl.putConstraint(SpringLayout.NORTH,l2,20,SpringLayout.SOUTH,l1);
				sl.putConstraint(SpringLayout.NORTH,tf2,20,SpringLayout.SOUTH,tf1);	
				sl.putConstraint(SpringLayout.NORTH,file2,20,SpringLayout.SOUTH,file1);	
				sl.putConstraint(SpringLayout.WEST,checkb,10,SpringLayout.WEST,p);	
				sl.putConstraint(SpringLayout.NORTH,checkb,30,SpringLayout.SOUTH,l2);
				
			}else{
				String[] list=new String[listOfObjects.size()];
				int maxLength=10;
				for(int i=0;i<listOfObjects.size();i++){
					list[i]=listOfObjects.get(i).toString();
					if(maxLength<list[i].length()){
						maxLength=list[i].length();
					}
				}			
				cb=new JComboBox(list);
				cb.setName(property2);
				cb.setEditable(false);					
				p.add(l2);
				p.add(cb);		
				sl.putConstraint(SpringLayout.WEST,l2,10,SpringLayout.EAST,tf1);
				sl.putConstraint(SpringLayout.WEST,cb,5,SpringLayout.EAST,l2);
				sl.putConstraint(SpringLayout.NORTH,l2,20,SpringLayout.NORTH,p);
				sl.putConstraint(SpringLayout.NORTH,cb,17,SpringLayout.NORTH,p);	
				
			}
			
		}					
		
		JButton ok=new JButton("Ok");
		ok.setActionCommand("Ok");
		ok.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		JButton cancel=new JButton("Cancel");
		cancel.setActionCommand("Cancel");
		cancel.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		p.add(ok);
		p.add(cancel);
		Spring con=sl.getConstraint(SpringLayout.WEST,tf1);
		
		
		if(property2!=null && property2.equals("File2:")){
			width-=200;
			sl.putConstraint(SpringLayout.NORTH, ok, 80,SpringLayout.SOUTH, tf2);
			sl.putConstraint(SpringLayout.NORTH, cancel, 80, SpringLayout.SOUTH, tf2);
			sl.putConstraint(SpringLayout.WEST, ok, Spring.sum(con,Spring.constant(80)),SpringLayout.WEST, p);		
			sl.putConstraint(SpringLayout.WEST, cancel, 10,SpringLayout.EAST, ok);
			height=250;
		}else{
			sl.putConstraint(SpringLayout.NORTH, ok, 50,SpringLayout.SOUTH, tf1);
			sl.putConstraint(SpringLayout.NORTH, cancel, 50, SpringLayout.SOUTH, tf1);
			sl.putConstraint(SpringLayout.WEST, ok, Spring.sum(con,Spring.constant(20)),SpringLayout.WEST, p);		
			sl.putConstraint(SpringLayout.WEST, cancel, 10,SpringLayout.EAST, ok);
		}
		
		
		
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(p,BorderLayout.CENTER);
		
		JFrame frame=parent;
		Point pos = frame.getLocationOnScreen();
		setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);		
		setSize(new Dimension(width, height)); 
		setLocationRelativeTo(frame);
		setVisible(true);	
		pack();
	}



	protected void windowAction(ActionEvent e) {
		boolean close=false;
		if(e.getActionCommand().equals("Ok")){
			if(tf1.getText()!=null && !tf1.getText().equals("")){
				if(tf2==null && cb!=null){
					
						if(cb.getName().contains("Molecule")){
							StringTokenizer st=new StringTokenizer(tf1.getText(),",");
							StringBuffer bf=new StringBuffer();
							bf.append("{");
							if(st.hasMoreTokens()){
								bf.append(st.nextToken());	
							}								
							while(st.hasMoreTokens()){
								bf.append(",");
								bf.append(st.nextToken());
							}
							bf.append("}");
							setOfObjectPatterns=bf.toString();
							bf=null;
							st=null;
						}else{
							setOfObjectPatterns=tf1.getText().trim();
						}
					//}				
					close=true;
				}else{
					setOfObjectPatterns=tf1.getText().trim();
					if(tf2==null){
						close=true;
					}else{
						if(tf2.getText()!=null && !tf2.getText().equals("")){
							molecule=tf2.getText().trim();
							if(checkb.isSelected()){
								setMergeOnly(true);
							}
							close=true;
						}else{
							JOptionPane.showMessageDialog(this,"You  have to define the second file also!","Warning", JOptionPane.WARNING_MESSAGE);
						}	
					}
									
				}			
				
			}else{
				JOptionPane.showMessageDialog(this,"Nothing set correctly!","Warning", JOptionPane.WARNING_MESSAGE);				
			}
			if(cb!=null && cb.getSelectedItem()!=null){
				molecule=cb.getSelectedItem().toString();
			}
		}else{
			close=true;
		}
		if(close){
			setVisible(false);
	        dispose();			
		}
		
	}

	public String getSetOfObjectPatterns() {
		return setOfObjectPatterns;
	}
	public void setSetOfObjectPatterns(String setOfObjectPatterns) {
		this.setOfObjectPatterns = setOfObjectPatterns;
	}
	public String getMolecule() {
		return molecule;
	}
	public void setMolecule(String molecule) {
		this.molecule = molecule;
	}
	public boolean isMergeOnly() {
		return mergeOnly;
	}
	public void setMergeOnly(boolean mergeOnly) {
		this.mergeOnly = mergeOnly;
	}
}
