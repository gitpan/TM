#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <topicmaps.h>
#include <tmmodel.h>

/* user data for VIEW callbacks */
struct user_data
{
	SV *user_data_ref;	/* Perl app's user data */
	SV *start;		/* Perl app's start callback */
	SV *end;		/* Perl app's end callback */
};

/* C callback for VIEW start events. Dispatches to the Perl
 * callback passed in via user data
 */
static int vstart(void* ud, const char *name, void **atts)
{
	SV *user_data_ref;
	SV *start;
	HV *perl_atts;	/* hash for passing atts to Perl callback */
	int i;
        struct user_data *up = (struct user_data*)ud;
	dSP ;
	start = up->start;
	user_data_ref = up->user_data_ref;
	perl_atts = newHV();

	/* For each (attribute,value,value type) tuple construct the
	 * corresponding entry in the hash 'perl_atts'.
	 */

	for (i = 0; atts && atts[i]; i += 3)
	{
		char *key;
		void *value;
		TMValueType vtype;

		key = (char*)atts[i];
		value = atts[i+1];
		vtype = (TMValueType)atts[i+2];
		if(strcmp(vtype->name,"Topic") == 0)
		{
			hv_store(perl_atts,
				key, strlen(key), newSViv( (int)value ) , 0);
		}
		else if(strcmp(vtype->name,"Text") == 0)
		{
			hv_store(perl_atts,
				key, strlen(key), newSVpv((char*)value,0) , 0);
		}
		else if(strcmp(vtype->name,"LocatorSet") == 0)
		{
			AV *perl_set;
			TMList lp;
			perl_set = newAV();
			for(lp= (TMList)value; lp; lp=lp->next)
			{
				av_push(perl_set,newSVpv((char*)lp->content,0));
			}
			hv_store(perl_atts,
				key, strlen(key), newRV_inc((SV*)perl_set),0);
		}
		else
		{
			warn("unknown value type '%s' in view event "
			     "attributes, cannot convert to Perl value. "
			     "Skiping attribute!", vtype->name );
		}
	}
	ENTER ;
	SAVETMPS ;
	PUSHMARK(SP) ;
	XPUSHs(sv_2mortal(newSVsv(user_data_ref)));
	XPUSHs(sv_2mortal(newSVpv(name, 0)));
	XPUSHs(sv_2mortal( newRV_inc( (SV*) perl_atts)) );
	PUTBACK ;

	call_sv(start,G_VOID);

	FREETMPS ;
	LEAVE ;
	return(0);
}
/* C callback for view end events. Dispatches to the Perl
 * callback passed in via user data
 */
static int vend(void* ud, const char *name)
{
	SV *user_data_ref;
	SV *end;
        struct user_data *up = (struct user_data*)ud;
	dSP ;
	end = up->end;
	user_data_ref = up->user_data_ref;
	ENTER ;
	SAVETMPS ;
	PUSHMARK(SP) ;
	XPUSHs(sv_2mortal(newSVsv(user_data_ref)));
	XPUSHs(sv_2mortal(newSVpv(name, 0)));
	PUTBACK ;

	call_sv(end,G_VOID);	/* call Perl callback */

	FREETMPS ;
	LEAVE ;
	return(0);
}
	
	

MODULE = TM		PACKAGE = TM::TopicMap

# Make sure that we have at least xsubpp version 1.922.
REQUIRE: 1.922

TMTopicMap
new(CLASS)
	char *CLASS
    PREINIT:
	TMTopicMap tm;
    CODE:
	tm = tm_topicmap_new(&default_mem_storage_descriptor);
	if( tm == NULL )
	{
		warn("unable to create TopicMap object");
		XSRETURN_UNDEF;
	}
	if( tm_topicmap_open(tm,NULL) != TM_OK)
	{
		warn("unable to open topic map, %s",
			tm_topicmap_get_error(tm));
		tm_topicmap_delete(&tm);
		XSRETURN_UNDEF;
	}
	RETVAL = tm;
    OUTPUT:
	RETVAL

void
DESTROY(self)
	TMTopicMap self
    CODE:
	tm_topicmap_close(self);	/* FIXME: return value */
	tm_topicmap_delete(&self);



int
require( self , val )
        TMTopicMap self
        char *val
    PREINIT:
	TMModel m;
	TMError e;
	int eins = 1;
    CODE:
	if( (e = tmtk_default_model_lookup_function(val,&m)) != TM_OK)
	{
		warn("unable to find model %s, %s", val, tm_strerror(e));
		XSRETURN_UNDEF;
	}
	if(tm_topicmap_require_model(self,m) != TM_OK)
	{
		warn("unable to load model %s to topic map, %s", val,
			tm_topicmap_get_error(self) );
		XSRETURN_UNDEF;
	}
	RETVAL = eins;
    OUTPUT:
	RETVAL


char*
get_error( self )
        TMTopicMap self
    PREINIT:
	char *s;
    CODE:
	s = (char*)tm_topicmap_get_error(self);
	RETVAL = s;
    OUTPUT:
	RETVAL


void
dump( self )
        TMTopicMap self
    CODE:
	tm_topicmap_dump(self,stdout);



void
load_file( self , fname , pm_name, parse_type)
        TMTopicMap self
        char *fname
	char *pm_name
	char *parse_type
    PREINIT:
	int parse_mode;
	TMProcModel pm;
	TMError e;
	int fd;
	struct stat stbuf;
	Omnivore o;
    CODE:
	if( strcmp(parse_type,"xml") == 0)
		parse_mode = TM_PARSE_XML;
	else if( strcmp(parse_type,"rdfxml") == 0)
		parse_mode = TM_PARSE_RDFXML;
	else if( strcmp(parse_type,"rdfntriples") == 0)
		parse_mode = TM_PARSE_RDFNTRIPLES;
	else if( strcmp(parse_type,"lines") == 0)
		parse_mode = TM_PARSE_LINES;
	else
	{
		warn("unknown input type %s",parse_type);
		assert(0);
	}

	if( (fd = open(fname,O_RDONLY,0)) < 0)
	{
		warn("cannot open %s, %s\n", fname, strerror(errno));
		assert(0);
	}
	if( fstat(fd,&stbuf) < 0)
	{
		warn("cannot stat %s, %s\n",
                                fname, strerror(errno));
	}
	if( (e = tmtk_default_procmodel_lookup_function(pm_name,&pm)) != TM_OK)
	{
		warn(tm_strerror(e));
		assert(0);
	}
 
	o = Omnivore_new(parse_mode,pm,self);
	for (;;)
	{
		char buf[4096];
		int len;
 
		if( (len = read(fd, buf, sizeof(buf))) < 0)
		{
			warn("error reading %s, %s\n", fname, strerror(errno));
			assert(0);
		}
 
		if (! Omnivore_parse(o, buf, len, len == 0))
		{
			warn("Parse error: %s\n", Omnivore_getErrorString(o) );
		}
		if (len == 0)
			break;
	}
	Omnivore_delete( &o );

void
query( self , user_data_ref, start, end, query )
        TMTopicMap self
	SV* user_data_ref
	SV* start
	SV* end
        char *query
    PREINIT:
	TMModel m;
	TMError e;
	struct user_data ud;
    CODE:
	ud.user_data_ref = user_data_ref;
	ud.start = start;
	ud.end = end;
	tm_topicmap_query(self,&ud,vstart,vend,query);

SV *
get_property(self,topic, fullname )
	TMTopicMap self
	int topic
	char *fullname
    PREINIT:
	char m[256];
	char *p;
	void *value;
	TMProperty prop;
	TMValueType vtype;
	TMError e;
    CODE:
	p = strstr(fullname,"::");
	assert(p);
	bzero(m,sizeof(m));
	strncpy(m,fullname, (p-fullname) );
	p += 2;  /* skip '::' */
	e = tm_topicmap_get_property(self, topic,m,p,&value,&prop);
	assert(e == TM_OK);
	
	if(!value)
		XSRETURN_UNDEF;
	vtype = prop->value_type;
	if(strcmp(vtype->name,"Topic") == 0)
	{
		RETVAL = newSViv( (int)value );
	}
	else if(strcmp(vtype->name,"Text") == 0)
	{
		RETVAL = newSVpv((char*)value,0);
	}
	else if(strcmp(vtype->name,"LocatorSet") == 0)
	{
		AV *perl_set;
		TMList lp;
		perl_set = newAV();
		for(lp= (TMList)value; lp; lp=lp->next)
		{
			av_push(perl_set, newSVpv((char*)lp->content, 0) );
		}
		RETVAL = newRV_inc( (SV*) perl_set);
	}
	else
	{
		warn("unknown value type");
		XSRETURN_UNDEF;
	}
    OUTPUT:
        RETVAL


SV *
get_topic(self,fullname,value )
	TMTopicMap self
	char *fullname
	void* value
    PREINIT:
	char m[256];
	char *p;
	TMProperty prop;
	TMTopic topic;
	TMError e;
    CODE:
	p = strstr(fullname,"::");
	assert(p);
	bzero(m,sizeof(m));
	strncpy(m,fullname, (p-fullname) );
	p += 2;  /* skip '::' */
	if( (e = tm_topicmap_get_topic(self, m,p,value,&topic,&prop)) != TM_OK)
	{
		/* FIXME: if prop, to_tring value and put in warning */
		warn("cannot get topic by %s, %s",fullname,
			tm_topicmap_get_error(self));
		XSRETURN_UNDEF;
	}
	if(topic == 0)
		XSRETURN_UNDEF;
	RETVAL = newSViv( (int)topic );
    OUTPUT:
        RETVAL




MODULE = TM			PACKAGE = TM

void
set_trace(mask)
	char *mask
        CODE:
        tm_set_trace_mask(mask);
