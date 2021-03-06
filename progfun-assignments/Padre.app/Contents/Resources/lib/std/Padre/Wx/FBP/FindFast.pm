package Padre::Wx::FBP::FindFast;

## no critic

# This module was generated by Padre::Plugin::FormBuilder::Perl.
# To change this module edit the original .fbp file and regenerate.
# DO NOT MODIFY THIS FILE BY HAND!

use 5.008;
use strict;
use warnings;
use Padre::Wx ();
use Padre::Wx::Role::Main ();

our $VERSION = '0.92';
our @ISA     = qw{
	Padre::Wx::Role::Main
	Wx::Panel
};

sub new {
	my $class  = shift;
	my $parent = shift;

	my $self = $class->SUPER::new(
		$parent,
		-1,
		Wx::DefaultPosition,
		Wx::DefaultSize,
		Wx::NO_BORDER | Wx::TAB_TRAVERSAL,
	);

	$self->{cancel} = Wx::Button->new(
		$self,
		-1,
		Wx::gettext("X"),
		Wx::DefaultPosition,
		Wx::DefaultSize,
	);

	Wx::Event::EVT_BUTTON(
		$self,
		$self->{cancel},
		sub {
			shift->close_clicked(@_);
		},
	);

	$self->{find_label} = Wx::StaticText->new(
		$self,
		-1,
		Wx::gettext("Find") . ":",
	);

	$self->{find_text} = Wx::TextCtrl->new(
		$self,
		-1,
		"",
		Wx::DefaultPosition,
		Wx::DefaultSize,
		Wx::TE_NO_VSCROLL,
	);

	Wx::Event::EVT_TEXT(
		$self,
		$self->{find_text},
		sub {
			shift->find_text_changed(@_);
		},
	);

	$self->{find_previous} = Wx::Button->new(
		$self,
		-1,
		Wx::gettext("Previous"),
		Wx::DefaultPosition,
		Wx::DefaultSize,
	);

	Wx::Event::EVT_BUTTON(
		$self,
		$self->{find_previous},
		sub {
			shift->find_previous_clicked(@_);
		},
	);

	$self->{find_next} = Wx::Button->new(
		$self,
		-1,
		Wx::gettext("Next"),
		Wx::DefaultPosition,
		Wx::DefaultSize,
	);
	$self->{find_next}->SetDefault;

	Wx::Event::EVT_BUTTON(
		$self,
		$self->{find_next},
		sub {
			shift->find_next_clicked(@_);
		},
	);

	$self->{find_case} = Wx::CheckBox->new(
		$self,
		-1,
		Wx::gettext("Match Case"),
		Wx::DefaultPosition,
		Wx::DefaultSize,
	);

	my $bSizer79 = Wx::BoxSizer->new(Wx::HORIZONTAL);
	$bSizer79->Add( 0, 0, 0, Wx::ALL | Wx::EXPAND, 5 );
	$bSizer79->Add( $self->{cancel}, 0, Wx::ALIGN_CENTER_VERTICAL | Wx::ALL, 5 );
	$bSizer79->Add( $self->{find_label}, 0, Wx::ALIGN_CENTER_VERTICAL | Wx::ALL, 5 );
	$bSizer79->Add( $self->{find_text}, 0, Wx::ALIGN_CENTER_VERTICAL | Wx::ALL, 5 );
	$bSizer79->Add( $self->{find_previous}, 0, Wx::ALIGN_CENTER_VERTICAL | Wx::ALL, 5 );
	$bSizer79->Add( $self->{find_next}, 0, Wx::ALIGN_CENTER_VERTICAL | Wx::ALL, 5 );
	$bSizer79->Add( $self->{find_case}, 1, Wx::ALIGN_CENTER_VERTICAL | Wx::ALL, 5 );
	$bSizer79->Add( 0, 0, 0, Wx::ALL | Wx::EXPAND, 5 );

	$self->SetSizerAndFit($bSizer79);
	$self->Layout;

	return $self;
}

sub find_text {
	$_[0]->{find_text};
}

sub find_previous {
	$_[0]->{find_previous};
}

sub find_next {
	$_[0]->{find_next};
}

sub find_case {
	$_[0]->{find_case};
}

sub close_clicked {
	$_[0]->main->error('Handler method close_clicked for event cancel.OnButtonClick not implemented');
}

sub find_text_changed {
	$_[0]->main->error('Handler method find_text_changed for event find_text.OnText not implemented');
}

sub find_previous_clicked {
	$_[0]->main->error('Handler method find_previous_clicked for event find_previous.OnButtonClick not implemented');
}

sub find_next_clicked {
	$_[0]->main->error('Handler method find_next_clicked for event find_next.OnButtonClick not implemented');
}

1;

# Copyright 2008-2011 The Padre development team as listed in Padre.pm.
# LICENSE
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl 5 itself.

