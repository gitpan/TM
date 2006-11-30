package TM::Tau::Federate;

use Data::Dumper;
use Time::HiRes;

# this is just a stub, much work to do here

use TM;
use base qw(TM);

sub new {
    my $class   = shift;
    my %self    = @_;

    $self{left}              or die "no left operand";
    $self{left}->isa ('TM')  or die "left operand no map";
    $self{right}             or die "no right operand";
    $self{right}->isa ('TM') or die "right operand no map";

    return $class->SUPER::new (%self);
}

sub left {
    my $self = shift;
    return $self->{left};
}

sub right {
    my $self = shift;
    return $self->{right};
}

sub mtime {
    my $self = shift;
#warn "mtime fed";
    my $mtimeleft  = $self->{left}->mtime;
    my $mtimeright = $self->{right}->mtime;
#warn "$mtimeleft  $mtimeright ";
    return $mtimeleft < $mtimeright ? $mtimeleft : $mtimeright;    # take the younger one
}

sub source_in {
    my $self = shift;

    $self->{left}->source_in;
    $self->{right}->source_in;
# here decide whether to materialize or not
#    $self->melt ($self->{left});
#    $self->add  ($self->{right});
#    $self->{melted} = 1;                                       # this indicates to later that local map is of significance
#    $self->consolidate;
}

sub sync_out {
    my $self = shift;
# will depend on ...
    $self->source_out; # do not think twice, just do it
}


#sub melt {
#    my $self = shift;#
#
#    die "not yet implemented";
#}

sub consolidate {
  my $self = shift;

  if ($self->{melted}) {
      $self->SUPER::consolidate;
  } else {                                  # we are virtual, not much we can do here except of propagation
      $self->{left}->consolidate (@_);
      $self->{right}->consolidate (@_);
  }
}

sub midlets {
  my $self = shift;
  die;
  if ($self->{melted}) {
      return $self->midlets (@_);
  } else { # no materialized sync_in has happened
      return _consolidate ($self->{left}->midlets (@_), $self->{right}->midlets (@_)); # TODO: merging on the fly? what is the basis?
  }
}

sub _consolidate {
    return @_;
}


sub midlet {
  my $self = shift;
  die;
  if ($self->{melted}) {
      return $self->midlet (@_);
  } else { # no materialized sync_in has happened
      return _consolidate ($self->{left}->midlet (@_), $self->{right}->midlet (@_)); # TODO: merging on the fly? what is the basis?
  }
}

sub mids {
    my $self = shift;

#warn "mids in federate";

    if (!$self->{created}) { # we are still in bootup mode
	return $self->SUPER::mids (@_);
    } elsif ($self->{melted}) {                                            # we synced and have locally merged map
warn "mids in federate, melted";
	return $self->SUPER::mids (@_);
    } else {                                                          # we have only two operands (filters)
warn "mids in federate, not melted";
	return
	    map {
		$_ ? $_                                               # 2) if it is ok, take it
		    : $self->{right}->mids ($_)                       # 3) if not try right map
		} $self->{left}->mids (@_);                           # 1) try it on the left map
    }
}

sub match {
    my $self = shift;

    die "not implemented yet";
}

1;
