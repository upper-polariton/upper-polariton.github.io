#!/usr/bin/perl -w

#
# This script reads an XPLOR input file with distance restraint data
# as sometimes is found in the pdb database (http://www.pdb.org).
# From this input file dihedral restrints should be removed, such that
# only distance restraints are left. The script can handle ambiguous restraints.
# It converts the distance restraints to GROMACS format.
# A typical command line would be
# ./xplor2gmx.pl 33 conf.pdb < restraints.dat > disre.itp
# You can turn off debugging info, but please do verify your output is correct!
#
# David van der Spoel (spoel@gromacs.org), July 2002
#

# Turn debugging off (0), or on ( > 0).
$debug = 1;
# Turn atom name translation on and off
$trans = 1;

$res0  = shift || die "I need the residue offset\n";
$pdb   = shift || die "I need the name of the pdb file with correct atom numbers\n";
$core  = shift || "core.ndx";
$tbl   = "/home/spoel/gmxdev/share/top/atom_nom.tbl";

printf "[ distance_restraints ]\n";
printf "; Read an xplor distance restraint file, and output GROMACS distance restraints.\n";
printf "; This also needs a pdb file with correct GROMACS atom numbering.\n";
printf "; I used $pdb for the atom numbers\n";
printf "; This also needs the offset in residues.\n";
printf "; I used $res0 for the residue offset\n";

# Read the pdb file
# If things go wrong, check whether your pdb file is read correctly.
$natom = 0;
$nres  = 0;
open(PDB,$pdb) || die "Can not open file $pdb\n";
while ($line = <PDB>) {
    if (index($line,"ATOM") >= 0) {
	@tmp = split(' ',$line);
	$aname[$natom] = $tmp[2];
	$resnr[$natom] = $tmp[4];
	if ($resnr[$natom] > $nres) {
	    $rname[$nres] = $tmp[3];
	    $nres++;
	}
	$natom++;
	
    }
}
close PDB;
printf "; I found $natom atoms in the pdb file $pdb\n";
printf "; I found $nres residues in the pdb file $pdb\n";
if ($debug > 1) {
    for ($i = 0; ($i < $natom); $i ++) {
	printf("; %5d  %5s  %5s  %5d\n",$i+1,$aname[$i],
	       $rname[$resnr[$i]-1],$resnr[$i]);
    }
}

#
# Read the name translation table.
# Source for this comes from: http://www.bmrb.wisc.edu/ref_info/atom_nom.tbl
# It was adapted slightly for GROMACS use, but only as regards formatting,
# not for content
#
open(TBL,$tbl) || die "Can not open atom-name translation table $tbl\n";
$ntbl=0;
while ($line = <TBL>) {
    @ttt = split('#',$line);
    @tmp = split(' ',$ttt[0]);
    if ($#tmp == 2) {
	# New table entry
	$tblres[$ntbl] = $tmp[0];
	$tblxplor[$ntbl] = $tmp[1];
	$tblgmx[$ntbl] = $tmp[2];
	$ntbl++;
    }
}
close TBL;
printf "; Read $ntbl entries from $tbl\n";

@templates = (
 [ "HH#",  "HH11",	"HH12",	"HH21",	"HH22" ],
 [ "HE#",  "HE",        "HE1",	"HE2"	],
 [ "HD2#", "HD21",	"HD22"	],
 [ "HD1#", "HD11",	"HD12"	],
 [ "HD#",  "HD1",	"HD2",	"HD11",	"HD12",	"HD13",	"HD21",	"HD22",	"HD23" ],
 [ "HG2#", "HG21",	"HG22",	"HG23"	],
 [ "HG1#", "HG11",	"HG12",	"HG13"	],
 [ "HG#",  "HG",         "HG1",	"HG2",	"HG11",	"HG12",	"HG13",	"HG21",	"HG22",	"HG23"  ],
 [ "HB#",  "HB",         "HB1",	"HB2"	],
 [ "HZ#",  "HZ",         "HZ1",	"HZ2",	"HZ3"	],
 [ "HN",   "H" ],
);

$ntranslated = 0;
$nuntransltd = 0;
sub transl_aname {
    my $atom   = shift(@_);
    my $resnr  = shift(@_);

    if ( $trans == 1 ) {    
	for(my $i = 0; ($i < $ntbl); $i ++) {
	    if ($tblres[$i] eq $rname[$resnr]) {
		if ($tblxplor[$i] eq $atom) {
		    $ntranslated++;
		    return $tblgmx[$i];
		}
	    }
	}
    }
    $nuntransltd++;
    if ($debug > 1) {
	printf "; No name change for $rname[$resnr] $atom\n";
    }
    return $atom;
}

sub expand_template {
    my $atom  = shift(@_);
    my $rnum  = shift(@_)-1;
    my $bdone = 0;
    my @atoms;
    my $jj = 0;
    
    for (my $tt=0; (($tt <= $#templates) && ($bdone == 0)); $tt++) {
	$templ = $templates[$tt];
	if ($atom eq $templ->[0]) {
	    for ($jj = 1; ($jj <= $#{$templ}); $jj++) {
		push @atoms, transl_aname($templ->[$jj],$rnum);
	    }
	    $bdone  = 1;
	}
    }
    if ($bdone == 0) {
	push @atoms, transl_aname($atom,$rnum);
    }
    if ($debug > 0) {
	my $natom = $#atoms+1;
	printf("; Found $natom elements for atom $rname[$rnum] %d $atom:",
	       $rnum+1);
	for $aa ( @atoms ) {
	    printf " $aa";
	}
	printf "\n";
    }
    return @atoms;
}

if ($debug > 1) {
    printf "; There are $#templates template entries\n";
    for ($tt=0; ($tt <= $#templates); $tt++) {
	$templ  = $templates[$tt];
	$ntempl = $#{$templ};
	printf "; Item $tt ($templates[$tt][0]) has $ntempl entries\n";
    }
}

# This index file holds numbers of restraints involving core atoms
@protcore = ( "H", "HN", "HA", "HA1", "HA2", "HB", "HB1", "HB2", "HB3", "HG", "HG1", "HG2", "HG3", "N", "O"  );
open(CORE,">$core") || die "Can not open $core\n";
print CORE "[ core-restraints ]\n";
$ncore = 0;

$myindex     = 0;
$linec       = 0;
$npair       = 0;
$nunresolved = 0;
while ($line = <STDIN>) {
    @ttt = split('!',$line);
    if ((index($ttt[0],"assign") >= 0) && (index($ttt[0],"!assign") < 0)) {
	@tmp = split('\(',$ttt[0]);
	# Find first argument
	if (($rhaak  = index($tmp[1],')')) < 0) {
	    printf "No ) in '$tmp[1]'\n";
	}
	$atres1 = substr($tmp[1],0,$rhaak);
	@at1 = split('or',$atres1);
	
	# Find second argument
	if (($rhaak  = index($tmp[2],')')) < 0) {
	    printf "No ) in '$tmp[2]'\n";
	}
	$atres2 = substr($tmp[2],0,$rhaak);
	@at2 = split('or',$atres2);

	@expdata = split('\)',$tmp[2]);
	@dist    = split(' ',$expdata[1]);

	$bOK   = 0;
	$bCore = 1;
	foreach $a1 ( @at1 ) {
	    @info1  = split(' ',$a1);
	    $r1     = $info1[1] - $res0;
	    @atoms1 = expand_template($info1[4],$r1);

	    foreach $a2 ( @at2 ) {
		@info2  = split(' ',$a2);
		$r2     = $info2[1] - $res0;
		@atoms2 = expand_template($info2[4],$r2);

		for ($i = 0; ($i < $natom) && ($resnr[$i] < $r1); $i++) { ; }
		for ( ; ($i < $natom) && ($resnr[$i] == $r1); $i++) { 
		    foreach $ii ( @atoms1 ) {
			if ($ii eq $aname[$i]) {
			    $bCoreI = 0;
			    for $pp ( @protcore ) {
				if ($ii eq $pp) {
				    $bCoreI = 1;
				}
			    }
					
			    for ($j = 0; ($j < $natom) && ($resnr[$j] < $r2); $j++) { ; }
			    for ( ; ($j < $natom) && ($resnr[$j] == $r2); $j++) { 
				foreach $jj ( @atoms2 ) {
				    if ($jj eq $aname[$j]) {
					$dd     = 0.1*$dist[0];
					$dminus = 0.1*$dist[1];
					$dplus  = 0.1*$dist[2];
					$low    = $dd-$dminus;
					$up1    = $dd+$dplus;
					$up2    = $up1+1;
					printf("%5d %5d %5d %5d %5d %7.3f %7.3f %7.3f 1.0; res $r1 $ii - res $r2 $jj\n",$i+1,$j+1,1,$myindex,1,$low,$up1,$up2);
					# Do some checks
					$bOK    = 1;
					$bCoreJ = 0;
					for $pp ( @protcore ) {
					    if ($jj eq $pp) {
						$bCoreJ = 1;
					    }
					}
					if (($bCoreI == 0) || ($bCoreJ == 0)) {
					    if ($debug > 0) {
						printf "; No core $ii ($bCoreI) $jj ($bCoreJ)\n";
					    }
					    $bCore = 0;
					}
					$npair++;
				    }
				}
			    }
			}
		    }
		}
	    }
	}
	if ($bCore == 1) {
	    printf CORE "$myindex\n";
	    $ncore++;
	}
	if ($bOK == 0) {
	    printf "; UNRESOLVED: $ttt[0]\n";
	    $nunresolved++;
	}
	$myindex++;
    }
    $linec++;
}
close CORE;

printf "; A total of $myindex lines with distance restraints were read from $linec input lines\n";
printf "; From this, $npair actual restraints were generated.\n";
printf "; A total of $nunresolved lines could not be interpreted\n";
printf "; In the process $ntranslated atoms had a name change\n";
printf "; However for $nuntransltd no name change could be found\n";
printf "; usually these are either HN->H renamings or not-existing atoms\n";
printf "; generated by the template expansion (e.g. HG#)\n";
printf "; A total of $ncore restraints were classified as core restraints\n";
