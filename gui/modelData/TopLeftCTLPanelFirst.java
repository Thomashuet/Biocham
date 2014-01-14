package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

public class TopLeftCTLPanelFirst extends JPanel{
	
	public JCheckBox whyOption, fairnessOption, reorderingOption, modeOption;
	
	public TopLeftCTLPanelFirst(CTLController listener){
		super();
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));		
		setBackground(Utils.backgroundColor);
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Checking CTL Properties"));	
		setName("Checking CTL Properties");
		String toolTipText="<html><i>Tests the adequacy of the model w.r.t. its specification,<br>" +
		  "and either for each unsatisfied CTL property, computes the <br>" +
		  "result of why[CHECK], or for each CTL property, computes the <br>" +
		  "result of why[CHECK_WHY], or summarizes the result with the first <br>" +
		  "unsatisfied property if there is one[CHECK_ALL]. </i></html>";
				
		CustomToolTipButton b3=new CustomToolTipButton("Check",toolTipText); 
		b3.setName("CheckCTL");
		b3.setActionCommand("checkCTL");
		b3.addActionListener(listener);
		b3.setBalloonToolTipVisible(false);
			
		toolTipText="<html><i>Evaluates a temporal query using the model-checker NuSMV.<br>" +
		"The first use of this command may take a while as it will <br>" +
		"compile the rules into an ordered binary decison diagram (OBDD).</i></html>";
		
		CustomToolTipButton b4=new CustomToolTipButton("Check CTL Property",toolTipText);   
		b4.setName("Nusmv");
		b4.setActionCommand("nusmv");
		b4.addActionListener(listener);
		b4.setBalloonToolTipVisible(false);
		
		add(Box.createRigidArea(new Dimension(20,10)));
		add(b3);
		add(Box.createRigidArea(new Dimension(20,10)));
		add(b4);
		add(Box.createRigidArea(new Dimension(20,20)));
		
		whyOption=new JCheckBox("Why");
		whyOption.setName("why");
		whyOption.setSelected(false);   
		fairnessOption=new JCheckBox("Fairness path");
		fairnessOption.setName("fairness");
		fairnessOption.setSelected(false);
		reorderingOption=new JCheckBox("Dynamic reordering");
		reorderingOption.setName("reordering");
		reorderingOption.setSelected(false);
		modeOption=new JCheckBox("Direct mode");
		modeOption.setName("mode");
		modeOption.setSelected(false);
		whyOption.setBackground(Utils.backgroundColor);
		fairnessOption.setBackground(Utils.backgroundColor);
		reorderingOption.setBackground(Utils.backgroundColor);
		modeOption.setBackground(Utils.backgroundColor);
		add(whyOption);
		add(fairnessOption);
		add(reorderingOption);
		add(modeOption);
		add(Box.createRigidArea(new Dimension(20,20)));
	}
}
