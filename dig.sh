#!/usr/bin/env bash
set -v
dig @localhost -p 1053 +tries=1 multiparty.session.types

